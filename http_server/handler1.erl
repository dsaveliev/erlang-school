-module(handler1).
-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).

init({tcp, http}, Req, Opts) ->
    {ok, Req, undefined_state}.
 
handle(Req, State) ->
    {ok, Req2} = cowboy_req:reply(200, [], <<"Hello from handler1">>, Req),
    {ok, Req2, State}.
 
terminate(Reason, Req, State) ->
    ok.