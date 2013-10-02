-module(client).

-export([start/0]).

start() ->
    inets:start(),
    {ok, {{_Version, 200, _Reason}, _Headers, Body}} =
        httpc:request("http://localhost:8080/user/?user_name=Bob"),
    Body.

