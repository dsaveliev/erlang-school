-module(dchat_app).
-author('Yura Zhloba <yzh44yzh@gmail.com>').

-behaviour(application).
-export([start/0, start/2, stop/1]).
-include("logger.hrl").

start() ->
    ok = application:start(crypto),
    ok = application:start(ranch),
    ok = application:start(cowboy),
    ok = application:start(dchat),
    ok.


start(_StartType, _StartArgs) ->
    Routes = cowboy_router:compile(routes()),
    cowboy:start_http(http, 100,
                      [{port, 8080}],
                      [{env, [{dispatch, Routes}]}]),
    dchat_sup:start_link().


stop(_State) ->
    ok = application:stop(cowboy),
    ok = application:stop(ranch),
    ok = application:stop(crypto),
    ok.


routes()->
    [{'_', static_routes() ++
          [
           %% {"/api/catalog/", catalog_handler, []},
           {'_', not_found_handler, []}
          ]}].


static_routes() ->
    PrivDir = "priv/static/",
    Mimetypes = {mimetypes, {fun mimetypes:path_to_mimes/2, default}},
    [{"/css/[...]", cowboy_static, [{directory, PrivDir ++ "css"}, Mimetypes]},
     {"/js/[...]",  cowboy_static, [{directory, PrivDir ++ "js"}, Mimetypes]},
     {"/img/[...]", cowboy_static, [{directory, PrivDir ++ "img"}, Mimetypes]}].
