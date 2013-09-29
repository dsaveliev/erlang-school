-module(kvs_app).
-author('Yura Zhloba <yzh44yzh@gmail.com>').

-behaviour(application).
-export([start/0, start/2, stop/1]).


start() ->
    inets:start(),
    ok = application:start(ranch),
    ok = application:start(kvs),
    ok.


start(_StartType, _StartArgs) ->
    {ok, Port} = application:get_env(port),
    {ok, NumAcceptors} = application:get_env(num_acceptors),
    io:format("~nkvs_app started~ntcp handler at port ~p with num acceptors ~p~n", [Port, NumAcceptors]),
    ranch:start_listener(tcp_pool, NumAcceptors,
                         ranch_tcp, [{port, Port}, {max_connections, 500000}],
                         kvs_tcp_handler, []),
    {ok, DbOptions} = application:get_env(db_options),
    kvs_sup:start_link(DbOptions).


stop(_State) ->
    ok.

