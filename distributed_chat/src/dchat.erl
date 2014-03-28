-module(dchat).
-author('Yura Zhloba <yzh44yzh@gmail.com>').

-behavior(gen_server).

-export([start_link/0, join_client/1, leave_client/1, broadcast_msg/1,
         get_online/0, get_history/0, cluster_event/1, sync/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("logger.hrl").
-include("common.hrl").


-define(HISTORY_LIMIT, 10).


-record(state, {
          online = [] :: [client()],
          history = [] :: [message()]
	 }).


%%% module API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


-spec(join_client(client()) -> ok).
join_client(Client) ->
    gen_server:cast(?MODULE, {join_client, Client, true}),
    ok.


-spec(leave_client(client()) -> ok).
leave_client(Client) ->
    gen_server:cast(?MODULE, {leave_client, Client, true}),
    ok.


-spec(broadcast_msg(message()) -> ok).
broadcast_msg(Message) ->
    gen_server:cast(?MODULE, {broadcast_msg, Message, true}),
    ok.


-spec(get_online() -> [user()]).
get_online() ->
    gen_server:call(?MODULE, get_online).


-spec(get_history() -> [message()]).
get_history() ->
    gen_server:call(?MODULE, get_history).


-spec(cluster_event(term()) -> ok).
cluster_event({user_join, User}) ->
    gen_server:cast(?MODULE, {join_client, {User, null}, false}),
    ok;
cluster_event({user_leave, User}) ->
    gen_server:cast(?MODULE, {leave_client, {User, null}, false}),
    ok;
cluster_event({msg, Message}) ->
    gen_server:cast(?MODULE, {broadcast_msg, Message, false}),
    ok;
cluster_event(Event) ->
    ?ERROR("unknown remote event ~p on node ~p", [Event, node()]),
    ok.

-spec(sync([user()], [message()]) -> ok).
sync(Online, History) ->
    gen_server:cast(?MODULE, {sync, Online, History}),
    ok.


%%% gen_server API

init([]) ->
    {ok, #state{}}.


handle_call(get_online, _From, #state{online = Online} = State) ->
    Reply = lists:map(fun({User, _Pid}) -> User end, Online),
    {reply, Reply, State};

handle_call(get_history, _From, #state{history = History} = State) ->
    Reply = lists:reverse(History),
    {reply, Reply, State};

handle_call(Any, _From, State) ->
    ?ERROR("unknown call ~p in ~p ~n", [Any, ?MODULE]),
    {noreply, State}.


handle_cast({join_client, {User, _} = Client, SendRemoteEvent}, #state{online = Online} = State) ->
    Online2 = case lists:member(Client, Online) of
                  true -> Online;
                  false -> [Client | Online]
              end,
    State2 = State#state{online = Online2},
    broadcast({user_join, User}, SendRemoteEvent, State2),
    {noreply, State2};

handle_cast({leave_client, {User, _} = Client, SendRemoteEvent}, #state{online = Online} = State) ->
    Online2 = lists:delete(Client, Online),
    State2 = State#state{online = Online2},
    broadcast({user_leave, User}, SendRemoteEvent, State2),
    {noreply, State2};

handle_cast({broadcast_msg, Message, SendRemoteEvent}, #state{history = History} = State) ->
    History2 = [Message | History],
    Len = length(History2),
    History3 = if
                   Len > ?HISTORY_LIMIT ->
                       [_Last | RHistory] = lists:reverse(History2),
                       lists:reverse(RHistory);
                   true -> History2
               end,
    State2 = State#state{history = History3},
    broadcast({msg, Message}, SendRemoteEvent, State2),
    {noreply, State2};

handle_cast({sync, Online, History}, State) ->
    Online2 = lists:map(fun(User) -> {User, null} end, Online),
    State2 = State#state{online = Online2, history = History},
    {noreply, State2};

handle_cast(Any, State) ->
    ?ERROR("unknown cast ~p in ~p ~n", [Any, ?MODULE]),
    {noreply, State}.


handle_info(show_state, State) ->
    ?INFO("~p", [State]),
    {noreply, State};

handle_info(Request, State) ->
    ?ERROR("unknown info ~p in ~p ~n", [Request, ?MODULE]),
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVersion, State, _Extra) ->
    {ok, State}.


%%% inner functions

broadcast(Event, SendRemoteEvent,  #state{online = Online}) ->
    ?INFO("broadcast ~p", [Event]),
    lists:map(fun({_User, null}) -> do_nothing;
                 ({_User, Pid}) -> Pid ! Event
              end, Online),
    if
        SendRemoteEvent ->
            Nodes = lists:delete(node(), nodes()),
            rpc:multicall(Nodes, dchat, cluster_event, [Event]);
        true -> do_nothing
    end,
    ok.
