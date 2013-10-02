-module(server2).

-export([start/0, stop/0]).

start() ->
    application:start(crypto),

    code:add_patha("deps/ranch/ebin"),
    application:start(ranch), 

    code:add_patha("deps/cowboy/ebin"),
    application:start(cowboy),

    Dispatch = cowboy_router:compile([
                                      %% {URIHost, list({URIPath, Handler, Opts})}
                                      {'_', [
                                             {"/user/[...]", handler2, []},
                                             {'_', handler1, []}
                                            ]}
                                     ]),

    cowboy:start_http(my_http_listener, 100,
                      [{port, 8080}],
                      [{env, [{dispatch, Dispatch}]}]
                     ).


stop() ->
    application:stop(cowboy),
    application:stop(ranch),
    application:stop(crypto).
