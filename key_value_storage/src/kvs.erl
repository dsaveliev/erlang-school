-module(kvs).
-author('Yura Zhloba <yzh44yzh@gmail.com>').

-behavior(gen_server).

-export([start_link/1, create/2, read/1, update/2, delete/1, clear/0, flush/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("kvs_types.hrl").

-type(value_set() :: [{calendar:datetime(), value()}]).
-type(item() :: {key(), value_set()}).

-record(state, {
          data_file :: string(),
          items = [] :: [item()]
	 }).


%%% module API

start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).


-spec(create(key(), value()) -> ok).
create(Key, Value) ->
    gen_server:call(?MODULE, {create, Key, Value}).


-spec(read(key()) -> {ok, value_set()} | {error, no_value}).
read(Key) ->
    gen_server:call(?MODULE, {read, Key}).


-spec(update(key(), value()) -> ok | {error, no_value}).
update(Key, NewValue) ->
    gen_server:call(?MODULE, {update, Key, NewValue}).


-spec(delete(key()) -> ok | {error, no_value}).
delete(Key) ->
    gen_server:call(?MODULE, {delete, Key}).


-spec(clear() -> ok).
clear() ->
    gen_server:call(?MODULE, clear).


-spec(flush() -> ok).
flush() ->    
    gen_server:cast(?MODULE, flush),
    ok.


    

%%% gen_server API

init(Options) ->
    io:format("kvs started with options: ~p~n", [Options]),
    self() ! restore,
    FlushInterval = proplists:get_value(flush_file, Options, 10000),
    DataFile = proplists:get_value(data_file, Options, "priv/data_file"),
    timer:apply_interval(FlushInterval, gen_server, cast, [?MODULE, flush]),
    {ok, #state{data_file = DataFile}}.

handle_call({create, Key, Value}, _From, #state{items = Items} = State) ->
    Now = {date(), time()},
    NewItems = case proplists:get_value(Key, Items) of
                   undefined -> ValueSet = [{Now, Value}],
                                [{Key, ValueSet} | Items];
                   ValueSet -> ValueSet2 = add_value_to_set(Now, Value, ValueSet),
                               replace_item({Key, ValueSet2}, {Key, ValueSet}, Items)
               end,
    {reply, ok, State#state{items = NewItems}};

handle_call({read, Key}, _From, #state{items = Items} = State) ->
    case proplists:get_value(Key, Items) of
        undefined -> {reply, {error, no_value}, State};
        ValueSet -> {reply, {ok, ValueSet}, State}
    end;

handle_call({update, Key, NewValue}, _From, #state{items = Items} = State) ->
    Now = {date(), time()},
    case proplists:get_value(Key, Items) of
        undefined -> {reply, {error, no_value}, State};
        ValueSet -> [_ | Rest] = ValueSet,
                    ValueSet2 = [{Now, NewValue} | Rest],
                    NewItems = replace_item({Key, ValueSet2}, {Key, ValueSet}, Items),
                    {reply, ok, State#state{items = NewItems}}
    end;

handle_call({delete, Key}, _From, #state{items = Items} = State) ->
    case proplists:get_value(Key, Items) of
        undefined -> {reply, {error, no_value}, State};
        _ -> NewItems = proplists:delete(Key, Items),
             {reply, ok, State#state{items = NewItems}}
    end;

handle_call(clear, _From, State) ->
    {reply, ok, State#state{items = []}};

handle_call(Any, _From, State) ->
    ?ERROR("unknown call ~p in ~p ~n", [Any, ?MODULE]),
    {noreply, State}.


handle_cast(flush, #state{data_file = DataFile, items = Items} = State) ->
    Data = term_to_binary(Items),
    file:write_file(DataFile, Data),
    {noreply, State};

handle_cast(Any, State) ->
    ?ERROR("unknown cast ~p in ~p ~n", [Any, ?MODULE]),
    {noreply, State}.


handle_info(restore, #state{data_file = DataFile} = State) ->
    case file:read_file(DataFile) of
        {ok, Data} -> Items = binary_to_term(Data),
                      {noreply, State#state{items = Items}};
        _ -> {noreply, State}
    end;

handle_info(Request, State) ->
    ?ERROR("unknown info ~p in ~p ~n", [Request, ?MODULE]),
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVersion, State, _Extra) ->
    {ok, State}.	


%% inner functions

-spec(add_value_to_set(calendar:datetime(), value(), value_set()) -> value_set()).
add_value_to_set(Time, Value, ValueSet) ->
    TimeVal = {Time, Value},
    case ValueSet of
        [Val1, Val2 | _] -> [TimeVal, Val1, Val2];
        _ -> [TimeVal | ValueSet]
    end.


-spec(replace_item(item(), item(), [item()]) -> [item()]).
replace_item(NewItem, OldItem, Items) ->
    [NewItem | lists:delete(OldItem, Items)].
