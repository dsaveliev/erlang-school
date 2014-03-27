-module(dchat_app).
-author('Yura Zhloba <yzh44yzh@gmail.com>').

-behaviour(application).
-export([start/0, start/2, stop/1]).
-include("logger.hrl").

start() ->
    lager:start(),
    ok = application:start(crypto),
    ok = application:start(ranch),
    ok = application:start(cowlib),
    ok = application:start(cowboy),
    ok = application:start(dchat),
    ?INFO("dchat app started"),
    ok.


start(_StartType, _StartArgs) ->
    {ok, Port} = application:get_env(port),
    Routes = cowboy_router:compile(routes()),
    cowboy:start_http(http, 100,
                      [{port, Port}],
                      [{env, [{dispatch, Routes}]}]),
    ?INFO("cowboy started at port ~p", [Port]),
    dchat_sup:start_link().


stop(_State) ->
    ok = application:stop(cowboy),
    ok = application:stop(ranch),
    ok = application:stop(crypto),
    ok.


routes()->
    [{'_',
      [
       {"/api/chat/", bullet_handler, [{handler, chat_handler}]},
       {"/", cowboy_static, {file, "priv/static/index.html", [{mimetypes, cow_mimetypes, all}]}},
       {"/[...]", cowboy_static, {dir, "priv/static", [{mimetypes, cow_mimetypes, all}]}},
       {'_', not_found_handler, []}
      ]}].
