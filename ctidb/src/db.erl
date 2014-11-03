-module(db).
-behavior(gen_server).

-export([start_link/1, add_value/2, get_value/1,
         delete_value/1, stop/0, flush/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("logger.hrl").
-include("otp_types.hrl").

-type(key() :: term()).
-type(value() :: term()).
-type(mode() :: production | test).

-record(state, {ets :: ets:tid(),
                mode = production :: mode()
               }).

%%% module API

-spec(start_link(mode()) -> gs_init_reply()).
start_link(Mode) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Mode, []).


-spec add_value(key(), value()) -> ok.
add_value(Key, Value) ->
    gen_server:cast(?MODULE, {add, Key, Value}),
    ok.


-spec get_value(key()) -> {ok, value()} | {error, not_found}.
get_value(Key) ->
    gen_server:call(?MODULE, {value, Key}).


-spec delete_value(key()) -> ok | not_found.
delete_value(Key) ->
    gen_server:call(?MODULE, {delete, Key}).


-spec stop() -> ok.
stop() ->
    gen_server:cast(?MODULE, stop),
    ok.


-spec flush() -> ok.
flush() ->
    ?MODULE ! flush_to_file,
    ok.


%%% gen_server API

-spec(init(gs_args()) -> gs_init_reply()).
init(Mode) ->
    ?INFO("Mode: ~p", [Mode]),
    Ets = ets:new(db_ets, [set, private]),
    case Mode of
        production ->
            FlushInterval = application:get_env(ctidb,
                                                db_flush_interval, 5000),
            ?INFO("flush interval ~p", [FlushInterval]),
            self() ! read_from_file,
            timer:send_interval(FlushInterval, flush_to_file);
        test -> do_nothing
    end,
    {ok, #state{ets = Ets, mode = Mode}}.


-spec(handle_call(gs_request(), gs_from(), gs_reply()) -> gs_call_reply()).
handle_call({value, Key}, _From, #state{ets = Ets} = State) ->
    Reply = case ets:lookup(Ets, Key) of
                [{Key, Value}] -> {ok, Value};
                _ -> {error, not_found}
            end,
    {reply, Reply, State};

handle_call({delete, Key}, _From, #state{ets = Ets} = State) ->
    Reply = case ets:lookup(Ets, Key) of
                [{Key, _Value}] -> ets:delete(Ets, Key), ok;
                _ -> not_found
            end,
    {reply, Reply, State};

handle_call(Any, _From, State) ->
    ?ERROR("unknown call ~p in ~p ~n", [Any, ?MODULE]),
    {noreply, State}.


-spec(handle_cast(gs_request(), gs_state()) -> gs_cast_reply()).
handle_cast({add, Key, Value}, #state{ets = Ets} = State) ->
    ets:insert(Ets, {Key, Value}),
    {noreply, State};

handle_cast(stop, State) ->
    ?INFO("db is stopping ~n"),
    {stop, normal, State};

handle_cast(Any, State) ->
    ?ERROR("unknown cast ~p in ~p ~n", [Any, ?MODULE]),
    {noreply, State}.


-spec(handle_info(gs_request(), gs_state()) -> gs_info_reply()).
handle_info(read_from_file, #state{ets = Ets, mode = production} = State) ->
    read_from_file(Ets),
    {noreply, State};

handle_info(flush_to_file, #state{ets = Ets, mode = production} = State) ->
    flush_to_file(Ets),
    {noreply, State};

handle_info(Request, State) ->
    ?ERROR("unknown info ~p in ~p ~n", [Request, ?MODULE]),
    {noreply, State}.


-spec(terminate(terminate_reason(), gs_state()) -> ok).
terminate(_Reason, #state{ets = Ets}) ->
    ?INFO("terminate, call flush_to_file ~n"),
    flush_to_file(Ets),
    ok.


-spec(code_change(term(), term(), term()) -> gs_code_change_reply()).
code_change(_OldVersion, State, _Extra) ->
    {ok, State}.



%%% inner functions

-spec read_from_file(ets:tid()) -> ok.
read_from_file(Ets) ->
    File = application:get_env(ctidb, db_flush_file, "priv/db"),
    case file:read_file(File) of
        {ok, Bin} ->
            Data = binary_to_term(Bin),
            ?INFO("read_from_file ~p", [Data]),
            lists:map(fun([Key, Value]) ->
                              ets:insert(Ets, {Key, Value})
                      end, Data);
        {error, _} -> ?ERROR("db, can't read data from file")
    end.


-spec flush_to_file(ets:tid()) -> ok | error.
flush_to_file(Ets) ->
    File = application:get_env(ctidb, db_flush_file, "priv/db"),
    Data = ets:match(Ets, {'$1', '$2'}),
    Res = file:write_file(File, term_to_binary(Data)),
    case Res of
        ok -> ok;
        {error, Reason} ->
            ?ERROR("db can't write data to file ~p ~p ~n", [File, Reason]),
            error
    end.
