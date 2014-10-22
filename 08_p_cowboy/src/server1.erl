-module(server1).
-behaviour(cowboy_http_handler).

-export([start/0]).
-export([init/3, handle/2, terminate/3]).

start() ->
    application:start(crypto),

    code:add_patha("deps/ranch/ebin"),
    application:start(ranch), 

    code:add_patha("deps/cowboy/ebin"),
    application:start(cowboy),

    Dispatch = cowboy_router:compile([
                                      %% {URIHost, list({URIPath, Handler, Opts})}
                                      {'_', [{'_', server1, []}]}
                                     ]),

    cowboy:start_http(my_http_listener, 100,
                      [{port, 8080}],
                      [{env, [{dispatch, Dispatch}]}]
                     ).


 
init({tcp, http}, Req, Opts) ->
    {ok, Req, undefined_state}.
 
handle(Req, State) ->
    {ok, Req2} = cowboy_req:reply(200, [], <<"Hello World!">>, Req),
    {ok, Req2, State}.
 
terminate(Reason, Req, State) ->
    ok.
