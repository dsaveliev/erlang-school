-module(kvs_tcp_handler).
-author('Yura Zhloba <yzh44yzh@gmail.com>').

-export([start_link/4, init/3]).

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
                         Data -> self() ! {send, Data}
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
                                                   
