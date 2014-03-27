-module(loop4).

-export([start/0, show_mailbox/1]).

start() ->
    io:format("start ~p~n", [self()]),
    spawn(fun loop/0).

loop() ->
    io:format("~p enters loop ~n", [self()]),
    receive
        msg1 -> io:format("~p got msg1 ~n", [self()]),
                loop();
        msg2 -> io:format("~p got msg2 ~n", [self()]),
                loop();
        stop -> stop
    after
        3000 ->
            io:format("~p no messages ~n", [self()]),
            loop()
    end.
            
show_mailbox(StrPid) when is_list(StrPid) ->
    show_mailbox(list_to_pid(StrPid));

show_mailbox(Pid) ->
    {messages, Msgs} = erlang:process_info(Pid, messages),
    Msgs.
