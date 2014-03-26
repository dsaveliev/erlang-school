-module(chat_handler).
-author('Yura Zhloba <yzh44yzh@gmail.com>').

-export([init/4, stream/3, info/3, terminate/2]).

-include("common.hrl").
-include("logger.hrl").


init(_Transport, Req, _Opts, _Active) ->
    ?INFO("chat_handler init"),
	{ok, Req, no_state}.


stream(<<"join">>, Req, State) ->
    Reply = io_lib:format("joined/~p", [node()]),
	{reply, Reply, Req, State};

stream(Data, Req, State) ->
	?INFO("chat received ~p~n", [Data]),
	{ok, Req, State}.


info(Info, Req, State) ->
	?INFO("info received ~p~n", [Info]),
	{ok, Req, State}.


terminate(_Req, _State) ->
	?INFO("chat terminate"),
	ok.
