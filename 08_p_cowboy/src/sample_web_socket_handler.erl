-module(sample_web_socket_handler).

-export([init/4, stream/3, info/3, terminate/2]).

init(_Transport, Req, _Opts, _Active) ->
	{ok, Req, []}.


stream(<<"join/", Name/binary>>, Req, State) ->
    Reply = lists:flatten(io_lib:format("joined/~s", [Name])),
	{reply, Reply, Req, State};

stream(<<"msg/", Text/binary>>, Req, State) ->
    self() ! {msg, Text},
	{ok, Req, State};

stream(_Data, Req, State) ->
	{ok, Req, State}.


info({msg, Text}, Req, State) ->
    Reply = lists:flatten(io_lib:format("msg/~s", [Text])),
	{reply, Reply, Req, State};

info(_Info, Req, State) ->
	{ok, Req, State}.


terminate(_Req, _State) ->
	ok.
