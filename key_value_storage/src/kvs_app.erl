-module(kvs_app).
-author('Yura Zhloba <yzh44yzh@gmail.com>').

-behaviour(application).
-export([start/0, start/2, stop/1]).


start() ->
    inets:start(),
    ok = application:start(crypto),
    ok = application:start(ranch),
    ok = application:start(cowboy),
    ok = application:start(kvs),
    ok.


start(_StartType, _StartArgs) ->
    {ok, TcpPort} = application:get_env(tcp_port),
    {ok, HttpPort} = application:get_env(http_port),
    {ok, NumAcceptors} = application:get_env(num_acceptors),
    io:format("~nkvs_app started~n"),
    io:format("tcp handler at port ~p~n", [TcpPort]),
    io:format("http handler at port ~p~n", [HttpPort]),
    ranch:start_listener(tcp_api, NumAcceptors,
                         ranch_tcp, [{port, TcpPort}, {max_connections, 500000}],
                         kvs_tcp_handler, []),

    Dispatch = cowboy_router:compile([
                                      %% {URIHost, list({URIPath, Handler, Opts})}
                                      {'_', [{'_', kvs_http_handler, []}]}
                                     ]),
    cowboy:start_http(http_api, NumAcceptors,
                      [{port, HttpPort}],
                      [{env, [{dispatch, Dispatch}]}]
                     ),

    {ok, DbOptions} = application:get_env(db_options),
    kvs_sup:start_link(DbOptions).


stop(_State) ->
    ok.

