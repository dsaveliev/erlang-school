-module(try_cowboy_app).

-behaviour(application).
-export([start/0, start/2, stop/1]).

start() ->
    ok = application:start(crypto),
    ok = application:start(ranch),
    ok = application:start(cowlib),
    ok = application:start(cowboy),
    ok = application:start(try_cowboy),
    ok.

start(_StartType, _StartArgs) ->
    {ok, Port} = application:get_env(port),
    Routing =
        [{'_',
          [
           {"/", cowboy_static, {file, "priv/static/index.html", [{mimetypes, cow_mimetypes, all}]}},
           {"/static/[...]", cowboy_static, {dir, "priv/static", [{mimetypes, cow_mimetypes, all}]}},
           {"/user/:user_id/profile/", sample_http_handler, []},
           {"/user/:name/messages", sample_template_handler, []},
           {"/session", sample_session_handler, []},
           {"/api/chat/", bullet_handler, [{handler, sample_web_socket_handler}]},
           {'_', not_found_handler, []}
          ]}],
    Routing2 = cowboy_router:compile(Routing),
    cowboy:start_http(http, 100,
                      [{port, Port}],
                      [{env, [{dispatch, Routing2}]}]),
    io:format("cowboy started at port ~p", [Port]),
    try_cowboy_sup:start_link().


stop(_State) ->
    ok = application:stop(cowboy),
    ok = application:stop(cowlib),
    ok = application:stop(ranch),
    ok = application:stop(crypto),
    ok.
