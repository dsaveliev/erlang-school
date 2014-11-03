-module(db_http_handler).
-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).
-include("logger.hrl").

init({tcp, http}, Req, Opts) ->
    {ok, Req, undefined_state}.


handle(Req, State) ->
    {Action, _} = cowboy_req:binding(action, Req),
    {Key, _} = cowboy_req:qs_val(<<"key">>, Req),
    ?INFO("handle ~p ~p", [Action, Key]),

    Body = case {Key, Action} of
               {undefined, _} -> "invalid request";
               {_, <<"add">>} ->
                   {Value, _} = cowboy_req:qs_val(<<"value">>, Req),
                   case Value of
                       undefined -> "invalid request";
                       _ -> db:add_value(Key, Value), "added"
                   end;
               {_, <<"get">>} ->
                   case db:get_value(Key) of
                       {ok, Val} -> io_lib:format("~p", [Val]);
                       {error, not_found} -> "not found"
                   end;
               {_, <<"delete">>} -> db:delete_value(Key), "deleted";
               {_, _} -> "invalid request"
           end,

    Headers = [{<<"Content-Type">>, <<"text/html;charset=UTF-8">>}],
    {ok, Req2} = cowboy_req:reply(200, Headers, Body, Req),
    {ok, Req2, State}.


terminate(Reason, Req, State) ->
    ok.
