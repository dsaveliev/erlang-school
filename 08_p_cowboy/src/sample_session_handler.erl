-module(sample_session_handler).
-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).


init({tcp, http}, Req, Opts) ->
    {ok, Req, undefined_state}.


handle(Req, State) ->
    {Sid, _} = cowboy_req:cookie(<<"sid">>, Req),
    {Body, Req3} = case Sid of
                       undefined ->
                           SessionId = generate_session_id(),
                           CookieProps = [{path, <<"/">>}, {max_age, 30 * 24 * 3600}], % 30 days
                           Req2 = cowboy_req:set_resp_cookie("sid", SessionId, CookieProps, Req),
                           {"session not found", Req2};
                       _ -> {["your session id is ", Sid], Req}
                   end,
    Headers = [{<<"Content-Type">>, <<"text/html;charset=UTF-8">>}],
    {ok, Req4} = cowboy_req:reply(200, Headers, Body, Req3),
    {ok, Req4, State}.


terminate(Reason, Req, State) ->
    ok.


generate_session_id() ->
    Str = integer_to_list(erlang:phash2({now(), make_ref()})),
    <<Hash:128/integer>> = erlang:md5(Str),
    list_to_binary(string:to_lower(integer_to_list(Hash, 16))).
