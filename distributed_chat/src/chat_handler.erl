-module(chat_handler).
-author('Yura Zhloba <yzh44yzh@gmail.com>').

-export([init/4, stream/3, info/3, terminate/2]).

-include("common.hrl").
-include("logger.hrl").

-record(state, {
          name :: binary()
         }).

init(_Transport, Req, _Opts, _Active) ->
    ?INFO("chat_handler init"),
	{ok, Req, #state{}}.


stream(<<"join/", Name/binary>>, Req, State) ->
    ?INFO("join ~p", [Name]),
    dchat:join_client({Name, self()}),
    Reply = io_lib:format("joined/~p", [node()]),
	{reply, Reply, Req, State#state{name = Name}};

stream(<<"msg/", Text/binary>>, Req, #state{name = Name} = State) ->
    dchat:broadcast_msg({Name, Text}),
	{ok, Req, State};

stream(Data, Req, State) ->
	?WARN("unknown stream ~p~n", [Data]),
	{ok, Req, State}.


info({user_join, Name}, Req, State) ->
    Reply = io_lib:format("user_join/~s", [Name]),
	{reply, Reply, Req, State};

info({user_leave, Name}, Req, State) ->
    Reply = io_lib:format("user_leave/~s", [Name]),
	{reply, Reply, Req, State};

info({msg, {Name, Text}}, Req, State) ->
    Reply = io_lib:format("msg/~s/~s", [Name, Text]),
	{reply, Reply, Req, State};

info(Info, Req, State) ->
	?WARN("unknown info  ~p~n", [Info]),
	{ok, Req, State}.


terminate(_Req, #state{name = Name}) ->
	?INFO("chat terminate"),
    dchat:leave_client({Name, self()}),
	ok.
