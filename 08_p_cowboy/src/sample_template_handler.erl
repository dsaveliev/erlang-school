-module(sample_template_handler).
-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).


init({tcp, http}, Req, Opts) ->
    {ok, Req, undefined_state}.


handle(Req, State) ->
    {UserName, _} = cowboy_req:binding(name, Req, <<"guest">>),

    {ok, Body} = sample_dtl:render([{name, UserName},
                                    {email, <<"some@where.com">>},
                                    {city, <<"Minsk">>},
                                    {messages, [{1, "Message 1"},
                                                {2, "Message 2"},
                                                {3, "Message 3"}]}]),
    Headers = [{<<"Content-Type">>, <<"text/html;charset=UTF-8">>}],
    {ok, Req2} = cowboy_req:reply(200, Headers, Body, Req),
    {ok, Req2, State}.


terminate(Reason, Req, State) ->
    ok.
