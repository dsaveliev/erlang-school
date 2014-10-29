-module(sample_web_socket_handler).

-export([init/4, stream/3, info/3, terminate/2]).

init(_Transport, Req, _Opts, _Active) ->
	{ok, Req, []}.


stream(<<"join/", Name/binary>>, Req, State) ->
    dchat:join_client({Name, self()}),
    Online = dchat:get_online(),
    Online2 = string:join(
                lists:map(fun(User) -> binary_to_list(User) end, Online),
                "|"),
    History = dchat:get_history(),
    History2 = string:join(
                 lists:map(fun({User, Text}) ->
                                   lists:flatten(io_lib:format("~s:~s", [User, Text]))
                           end, History),
                 "|"),
    Reply = lists:flatten(io_lib:format("joined/~s/~s/~s", [node(), Online2, History2])),
	{reply, Reply, Req, State};

stream(<<"msg/", Text/binary>>, Req, State) ->
	{ok, Req, State};

stream(Data, Req, State) ->
	{ok, Req, State}.


info({user_join, Name}, Req, State) ->
    Reply = lists:flatten(io_lib:format("user_join/~s", [Name])),
	{reply, Reply, Req, State};

info({user_leave, Name}, Req, State) ->
    Reply = lists:flatten(io_lib:format("user_leave/~s", [Name])),
	{reply, Reply, Req, State};

info({msg, {Name, Text}}, Req, State) ->
    Reply = lists:flatten(io_lib:format("msg/~s/~s", [Name, Text])),
	{reply, Reply, Req, State};

info(Info, Req, State) ->
	{ok, Req, State}.


terminate(_Req, State) ->
	ok.
