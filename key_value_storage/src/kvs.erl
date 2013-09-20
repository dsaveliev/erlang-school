-module(kvs).
-author('Yura Zhloba <yzh44yzh@gmail.com>').

-behavior(gen_server).

-export([start_link/1, create/2, read/1, update/2, delete/1, clear/0, flush/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("kvs_types.hrl").

-type(item() :: {name(), info()}).
-type(letter() :: {integer(), [item()]}).

-record(state, {
          data_file :: string(),
          letters = [] :: [letter()]
	 }).


%%% module API

start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).


-spec(create(name(), info()) -> ok | {error, name_exists}).
create(Name, Info) ->
    gen_server:call(?MODULE, {create, Name, Info}).


-spec(read(name()) -> {ok, info()} | {error, no_name}).
read(Name) ->
    gen_server:call(?MODULE, {read, Name}).


-spec(update(name(), info()) -> ok | {error, no_name}).
update(Name, NewInfo) ->
    gen_server:call(?MODULE, {update, Name, NewInfo}).


-spec(delete(name()) -> ok | {error, no_name}).
delete(Name) ->
    gen_server:call(?MODULE, {delete, Name}).


-spec(clear() -> ok).
clear() ->
    gen_server:call(?MODULE, clear).


-spec(flush() -> ok).
flush() ->    
    gen_server:cast(?MODULE, flush),
    ok.


    

%%% gen_server API

init(Options) ->
    io:format("db worker started with options: ~p~n", [Options]),
    self() ! restore,
    FlushInterval = proplists:get_value(flush_file, Options, 10000),
    DataFile = proplists:get_value(data_file, Options, "priv/data_file"),
    timer:apply_interval(FlushInterval, gen_server, cast, [?MODULE, flush]),
    {ok, #state{data_file = DataFile}}.

handle_call({create, Name, Info}, _From, #state{letters = Letters} = State) ->
    [FLetter | _] = Name,
    case proplists:get_value(FLetter, Letters) of
        undefined -> NewLetter = {FLetter, [{Name, Info}]},
                     NewLetters = [NewLetter | Letters],
                     {reply, ok, State#state{letters = NewLetters}};
        Items -> 
            case proplists:get_value(Name, Items) of
                undefined -> NewItems = [{Name, Info} | Items],
                             NewLetters = [{FLetter, NewItems} | proplists:delete(FLetter, Letters)],
                             {reply, ok, State#state{letters = NewLetters}};
                _Info -> {reply, {error, name_exists}, State}
            end
    end;

handle_call({read, Name}, _From, #state{letters = Letters} = State) ->
    [FLetter | _] = Name,
    case proplists:get_value(FLetter, Letters) of
        undefined -> {reply, {error, no_name}, State};
        Items ->
            case proplists:get_value(Name, Items) of
                undefined -> {reply, {error, no_name}, State};
                Info -> {reply, {ok, Info}, State}
            end
    end;

handle_call({update, Name, NewInfo}, _From, #state{letters = Letters} = State) ->
    [FLetter | _] = Name,
    case proplists:get_value(FLetter, Letters) of
        undefined -> {reply, {error, no_name}, State};
        Items ->
            case proplists:get_value(Name, Items) of
                undefined -> {reply, {error, no_name}, State};
                _ -> NewItems = [{Name, NewInfo} | proplists:delete(Name, Items)],
                     NewLetters = [{FLetter, NewItems} | proplists:delete(FLetter, Letters)],
                     {reply, ok, State#state{letters = NewLetters}}
            end
    end;

handle_call({delete, Name}, _From, #state{letters = Letters} = State) ->
    [FLetter | _] = Name,
    case proplists:get_value(FLetter, Letters) of
        undefined -> {reply, {error, no_name}, State};
        Items ->
            case proplists:get_value(Name, Items) of
                undefined -> {reply, {error, no_name}, State};
                _ -> NewItems = proplists:delete(Name, Items),
                     NewLetters = [{FLetter, NewItems} | proplists:delete(FLetter, Letters)],
                     {reply, ok, State#state{letters = NewLetters}}
            end
    end;

handle_call(clear, _From, State) ->
    {reply, ok, State#state{letters = []}};

handle_call(Any, _From, State) ->
    ?ERROR("unknown call ~p in ~p ~n", [Any, ?MODULE]),
    {noreply, State}.


handle_cast(flush, #state{data_file = DataFile, letters = Letters} = State) ->
    Data = term_to_binary(Letters),
    file:write_file(DataFile, Data),
    {noreply, State};

handle_cast(Any, State) ->
    ?ERROR("unknown cast ~p in ~p ~n", [Any, ?MODULE]),
    {noreply, State}.


handle_info(restore, #state{data_file = DataFile} = State) ->
    case file:read_file(DataFile) of
        {ok, Data} -> Letters = binary_to_term(Data),
                      {noreply, State#state{letters = Letters}};
        _ -> {noreply, State}
    end;

handle_info(Request, State) ->
    ?ERROR("unknown info ~p in ~p ~n", [Request, ?MODULE]),
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVersion, State, _Extra) ->
    {ok, State}.	

