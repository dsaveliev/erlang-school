-module(kvs_tcp_handler).
-author('Yura Zhloba <yzh44yzh@gmail.com>').

-export([start_link/4, init/3]).
-include("kvs_types.hrl").

-record(state, {
	  listener :: pid(),
	  socket :: socket(),
	  transport :: transport()
	 }).

-type(socket() :: pid()).
-type(transport() :: ranch_tcp | ranch_ssl).

%%% module API
-spec(start_link(pid(), socket(), transport(), [term()]) -> {ok, pid()}).
start_link(Listener, Socket, Transport, _Opts) ->
    Pid = spawn_link(?MODULE, init, [Listener, Socket, Transport]),
    {ok, Pid}.


-spec(init(pid(), socket(), transport()) -> no_return()).
init(Listener, Socket, Transport) ->
    ok = ranch:accept_ack(Listener),
    io:format("connection accepted~n"),
    self() ! read_data,
    loop(#state{listener = Listener, socket = Socket, transport = Transport}).


-spec(loop(#state{}) -> no_return()).
loop(#state{socket = Socket, transport = Transport} = State) ->
    receive
        read_data -> case read_package(State, []) of
                         closed -> self() ! close;
                         [] -> self() ! read_data;
                         Data -> Reply = process_data(Data),
                                 self() ! {send, Reply}
                     end,
                     loop(State);
        {send, Data} -> Transport:send(Socket, Data),
                        self() ! read_data,
                        loop(State);
        close -> io:format("connection closed~n"),
                 Transport:close(Socket),
                 stop;
        Any -> io:format("unknown msg ~p in ~p ~p ~n", [Any, ?MODULE, self()]),
               loop(State)
    end.


-spec(read_package(#state{}, [byte()]) -> closed | [byte()]).
read_package(#state{transport = Transport, socket = Socket} = State, Accum) ->
    case Transport:recv(Socket, 1, 200) of 
        {ok,  <<Byte:8/integer>>} -> read_package(State, [Byte | Accum]);
        {error, timeout} -> lists:reverse(Accum);
        {error, closed} -> closed
    end.
                                      

-spec(process_data([byte()]) -> [byte()]).
process_data(Data) ->
    Res = case parse_query(Data) of
              {create, Key, Value} -> kvs:create(Key, Value);
              {read, Key} -> kvs:read(Key);
              {update, Key, Value} -> kvs:update(Key, Value);
              {delete, Key} -> kvs:delete(Key);
              error -> "invalid query"
          end,
    io_lib:format("~p\n", [Res]).
            

-spec(parse_query([byte()]) -> {create, key(), value()} |
                               {read, key()} |
                               {update, key(), value()} |
                               {delete, key()} |
                               error).
parse_query(Data) ->
    Data1 = case lists:reverse(Data) of
                [10, 13 | Rest] -> lists:reverse(Rest);
                [13 | Rest] -> lists:reverse(Rest);
                [10 | Rest] -> lists:reverse(Rest);
                _ -> Data
            end,
    case string:tokens(Data1, " ") of
        ["create" | Tokens] when length(Tokens) > 1 ->
            [Key | Value] = Tokens,
            {create, Key, string:join(Value, " ")};
        ["read" | Tokens] when length(Tokens) > 0 ->
            [Key | _] = Tokens,
            {read, Key};
        ["update" | Tokens] when length(Tokens) > 1 ->
            [Key | Value] = Tokens,
            {update, Key, string:join(Value, " ")};
        ["delete" | Tokens] when length(Tokens) > 0 ->
            [Key | _] = Tokens,
            {delete, Key};
        _ -> error
    end.
                
            
            
            
        
    
