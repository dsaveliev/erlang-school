-module(server).

-export([start/1, listen/1, accept/1]).

start(Port) ->
    spawn(?MODULE, listen, [Port]).

listen(Port) ->
    io:format("server listens port ~p~n", [Port]),
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {active, true}]),
    spawn(?MODULE, accept, [ListenSocket]),
    timer:sleep(infinity),
    ok.

accept(ListenSocket) ->
    io:format("waiting for new client~n"),
    {ok, _Socket} = gen_tcp:accept(ListenSocket),
    spawn(?MODULE, accept, [ListenSocket]),
    handle().

handle() ->
    receive
        {tcp, Socket, <<"quit", _/binary>>} ->
            io:format("close client connection~n"),
            gen_tcp:close(Socket);
        {tcp, Socket, Msg} ->
            io:format("handle ~p~n", [Msg]),
            gen_tcp:send(Socket, Msg),
            handle()
    end.

