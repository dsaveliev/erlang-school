-module(tcp_handler).
-behaviour(ranch_protocol).

-export([start_link/4]).
-export([init/4]).

-include("logger.hrl").

start_link(Ref, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
    {ok, Pid}.

init(Ref, Socket, Transport, _Opts = []) ->
    ok = ranch:accept_ack(Ref),
    loop(Socket, Transport).

-spec loop(term(), term()) -> node().
loop(Socket, Transport) ->
    case Transport:recv(Socket, 0, 5000) of
        {ok, <<"quit", _/binary>>} -> Transport:close(Socket);
        {ok, <<"add ", KV/binary>>} ->
            Reply = case parse_tokens(KV) of
                        [Key, Value | _] ->
                            db:add_value(Key, Value),
                            <<"added">>;
                        _ -> <<"invalid command">>
                    end,
            Transport:send(Socket, <<Reply/binary, "\r\n">>),
            loop(Socket, Transport);
        {ok, <<"get ", K/binary>>} ->
            Reply = case parse_tokens(K) of
                        [Key | _] ->
                            case db:get_value(Key) of
                                {ok, Val} -> Val;
                                {error, not_found} -> <<"not found">>
                            end;
                        _ -> <<"invalid command">>
                    end,
            Transport:send(Socket, <<Reply/binary, "\r\n">>),
            loop(Socket, Transport);
        {ok, <<"del ", K/binary>>} ->
            Reply = case parse_tokens(K) of
                        [Key | _] ->
                            db:delete_value(Key),
                            <<"deleted">>;
                        _ -> <<"invalid command">>
                    end,
            Transport:send(Socket, <<Reply/binary, "\r\n">>),
            loop(Socket, Transport);
        {ok, Data} ->
            ?INFO("tcp_handler, unknown command ~p", [Data]),
            Transport:send(Socket, <<"unknown command\r\n">>),
            loop(Socket, Transport);
        {error, timeout} -> loop(Socket, Transport);
        Other -> ?INFO("tcp_handler:loop, got ~p", [Other]),
                 ok = Transport:close(Socket)
    end.

-spec parse_tokens(binary()) -> [binary()].
parse_tokens(Bin) ->
    Str = unicode:characters_to_list(Bin),
    Tokens = string:tokens(Str, " \n\r"),
    lists:map(fun unicode:characters_to_binary/1, Tokens).
