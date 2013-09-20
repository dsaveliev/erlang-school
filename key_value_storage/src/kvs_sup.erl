-module(kvs_sup).
-author('Yura Zhloba <yzh44yzh@gmail.com>').

-behaviour(supervisor).

-export([start_link/1, init/1]).

start_link(DbOptions) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [DbOptions]).


init([DbOptions]) ->
    RestartStrategy = one_for_one, % one_for_one | one_for_all | rest_for_one
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 60,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent, % permanent | transient | temporary
    Shutdown = 2000,     % brutal_kill | int() >= 0 | infinity
    Type = worker,       % worker | supervisor

    Kvs = {kvs, 
           {kvs, start_link, [DbOptions]},
           Restart, Shutdown, Type, 
           [kvs]}, 

    {ok, {SupFlags, [Kvs]}}.

