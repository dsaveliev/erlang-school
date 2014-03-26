-module(chat_handler).
-author('Yura Zhloba <yzh44yzh@gmail.com>').

-export([init/4, stream/3, info/3, terminate/2]).

-include("common.hrl").
-include("logger.hrl").


init(_Transport, Req, _Opts, _Active) ->
    ?INFO("chat_handler init"),
	TRef = erlang:send_after(5000, self(), refresh),
	{ok, Req, TRef}.


stream(Data, Req, State) ->
	?INFO("chat received ~p~n", [Data]),
	{ok, Req, State}.


info(refresh, Req, _State) ->
	TRef = erlang:send_after(5000, self(), refresh),
	DateTime = cowboy_clock:rfc1123(),
	?INFO("chat refresh: ~p~n", [DateTime]),
	{reply, DateTime, Req, TRef};

info(Info, Req, State) ->
	?INFO("info received ~p~n", [Info]),
	{ok, Req, State}.


terminate(_Req, TRef) ->
	?INFO("chat terminate"),
	erlang:cancel_timer(TRef),
	ok.
