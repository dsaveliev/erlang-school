-module(kvs_app).
-author('Yura Zhloba <yzh44yzh@gmail.com>').

-behaviour(application).
-export([start/0, start/2, stop/1]).


start() ->
    inets:start(),
    ok = application:start(kvs),
    ok.


start(_StartType, _StartArgs) ->
    %% {ok, Port} = application:get_env(port),
    {ok, DbOptions} = application:get_env(db_options),
    io:format("~nkvs_app started with options:~p~n", [DbOptions]),
    kvs_sup:start_link(DbOptions).


stop(_State) ->
    ok.

