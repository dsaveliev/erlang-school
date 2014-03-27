-module(my_cool_project_sup).
-author('Yura Zhloba <yzh44yzh@gmail.com>').

-behaviour(supervisor).

-export([start_link/0, init/1]).
-include("logger.hrl").
-include("types.hrl").

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    ?INFO("supervisor started ~p~n", [self()]),
    RestartStrategy = one_for_one, % one_for_one | one_for_all | rest_for_one
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 60,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent, % permanent | transient | temporary
    Shutdown = 2000,     % brutal_kill | int() >= 0 | infinity

    MyServer = {my_server,
		  {my_server, start_link, []}, 
		  Restart, Shutdown, worker, 
		  [my_server]},

    {ok, {SupFlags, [MyServer]}}.

