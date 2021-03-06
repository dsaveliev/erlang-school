-module(not_found_handler).

-export([init/3, handle/2, terminate/3]).

-include("logger.hrl").
-include("common.hrl").


%%% cowboy handler API

init(_Transport, Req, []) ->
    {ok, Req, undefined}.


handle(Req0, State) ->
    ?WARN("not found ~p", [element(1, cowboy_req:path(Req0))]),
    {ok, Req1} = cowboy_req:reply(404,
                                  [{<<"Content-Type">>, <<"text/html;charset=UTF-8">>}],
                                  "Not Found", Req0),
    {ok, Req1, State}.


terminate(_Reason, _Req, _State) ->
    ok.
