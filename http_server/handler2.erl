-module(handler2).
-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).

init({tcp, http}, Req, Opts) ->
    {ok, Req, undefined_state}.
 
handle(Req, State) ->
    {UserName, Req2} = cowboy_req:qs_val(<<"user_name">>, Req, <<"guest">>),
    {Host, _} = cowboy_req:host(Req),
    {Path, _} = cowboy_req:path(Req),
    {Url, _} = cowboy_req:url(Req),
    {QueryString, _} = cowboy_req:qs(Req),
    Body = [<<"<h1>Hello, ">>, UserName, <<"</h1>">>,
            <<"<p>Host: ">>, Host, <<"</p>">>,
            <<"<p>Path: ">>, Path, <<"</p>">>,
            <<"<p>Url: ">>, Url, <<"</p>">>,
            <<"<p>QueryString: ">>, QueryString, <<"</p>">>
           ],
    {ok, Req3} = cowboy_req:reply(200, [], Body, Req2),
    {ok, Req3, State}.
 
terminate(Reason, Req, State) ->
    ok.
