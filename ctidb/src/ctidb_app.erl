-module(ctidb_app).
-behaviour(application).

-export([start/0, start/2, stop/1, prep_stop/1]).
-include("logger.hrl").


-spec(start() -> ok).
start() ->
    lager:start(),
    application:start(crypto),
    application:start(ranch),
    application:start(cowlib),
    application:start(cowboy),
    application:start(ctidb),
    ok.


-spec(start(term(), term()) -> {ok, pid()}).
start(_StartType, _StartArgs) ->
    ?INFO("start citdb"),
    TcpPort = application:get_env(ctidb, tcp_api_port, 2000),
    ?INFO("start TCP API at port ~p", [TcpPort]),
    ranch:start_listener(tcp_api_pool, 20,
                         ranch_tcp, [{port, TcpPort}],
                         tcp_handler, []),

    HttpPort = application:get_env(ctidb, http_api_port, 8080),
    ?INFO("start HTTP API at port ~p", [HttpPort]),
    Routing =
        [{'_',
          [
           {"/db/:action/", db_http_handler, []}
          ]}],
    Routing2 = cowboy_router:compile(Routing),
    cowboy:start_http(http, 20,
                      [{port, HttpPort}],
                      [{env, [{dispatch, Routing2}]}]),
    ctidb_sup:start_link().


-spec prep_stop(term()) -> ok.
prep_stop(_State) ->
    db:flush(),
    ok.

-spec(stop(term()) -> ok).
stop(_State) ->
    ok.
