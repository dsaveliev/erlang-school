-module(my_server).
-author('Yura Zhloba <yzh44yzh@gmail.com>').

-behavior(gen_server).

-export([start_link/0, stop/0, add_stat/2, get_stat/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("logger.hrl").
-include("types.hrl").

-type(command() :: create | read | update | delete).
-type(command_info() :: {command(), [integer()]}).
-type(command_stat() :: {integer(), integer(), float()}).
-type(state() :: [command_info()]).

%%% module API

start_link() ->
    ?INFO("start_link ~n"),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, stop).

-spec(add_stat(command(), integer()) -> ok).
add_stat(Command, Time) ->
    gen_server:cast(?MODULE, {add_stat, Command, Time}),
    ok.
    
-spec(get_stat(command()) -> command_stat() | {error, not_found}).
get_stat(Command) ->
    gen_server:call(?MODULE, {get_stat, Command}).


%%% gen_server API

-spec(init(list()) -> {ok, state()}).
init([]) ->
    ?INFO("my_server started ~p~n", [self()]),
    {ok, []}.


handle_call({get_stat, Command}, _From, State) ->
    Reply = case find_command(Command, State) of
                {ok, {_Command, Times}} ->
                    Min = lists:min(Times),
                    Max = lists:max(Times),
                    Total = lists:foldl(fun(Time, Acc) ->
                                                Time + Acc
                                        end, 0, Times),
                    Avg = Total / length(Times),
                    {Min, Max, Avg};
                {error, not_found} -> {error, not_found}
            end,
    {reply, Reply, State};

handle_call(Any, _From, State) ->
    ?ERROR("unknown call ~p in ~p ~n", [Any, ?MODULE]),
    {noreply, State}.


handle_cast({add_stat, Command, Time}, State) ->
    State3 =
        case find_command(Command, State) of
            {error, not_found} ->
                NewCommandInfo = {Command, [Time]},
                [NewCommandInfo | State];
            {ok, {Command, Times}} ->
                NewCommandInfo = {Command, [Time | Times]},
                State1 = lists:delete({Command, Times}, State),
                [NewCommandInfo | State1]
        end,
    {noreply, State3};

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(Any, State) ->
    ?ERROR("unknown cast ~p in ~p ~n", [Any, ?MODULE]),
    {noreply, State}.


handle_info(show_state, State) ->
    ?INFO("State: ~w~n", [State]),
    {noreply, State};

handle_info(Request, State) ->
    ?ERROR("unknown info ~p in ~p ~n", [Request, ?MODULE]),
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVersion, State, _Extra) ->
    {ok, State}.	


%% inner functions

find_command(Command, State) ->
    Filtered = lists:filter(fun({CurrentCommand, _}) ->
                                    CurrentCommand =:= Command
                            end, State),
    case Filtered of
        [] -> {error, not_found};
        [CommandInfo] -> {ok, CommandInfo}
    end.
             
