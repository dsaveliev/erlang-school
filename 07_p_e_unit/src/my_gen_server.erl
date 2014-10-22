-module(my_gen_server).
-author('Yura Zhloba <yzh44yzh@gmail.com>').

-behavior(gen_server).

-export([start_link/0, add_user/1, remove_user/1, get_all_users/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(ERROR(Format, Data), error_logger:error_msg(Format, Data)).

-record(user, {
          id :: integer(),
          name :: binary(),
          age :: integer(),
          level :: integer()
         }).

-record(state, {
          users = [] :: [#user{}]
	 }).

%%% module API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


-spec(add_user(#user{}) -> {ok, added} | {error, already_exists}).
add_user(User) ->
    gen_server:call(?MODULE, {add_user, User}).


-spec(remove_user(UserId::integer()) ->
             {ok, removed} | {error, not_found}). 
remove_user(UserId) ->
    gen_server:call(?MODULE, {remove_user, UserId}).


-spec(get_all_users() -> [#user{}]).
get_all_users() ->
    gen_server:call(?MODULE, get_all_users).


%%% gen_server API

init([]) ->
    io:format("start ~p~n", [self()]),
    {ok, #state{}}.


handle_call({add_user, User}, _From,
            #state{users = Users} = State) ->
    Found = lists:filter(fun(#user{id = Id}) ->
                                 Id =:= User#user.id
                         end, Users),
    {Reply, NState} = case Found of
                          [] -> NewUsers = [User | Users],
                                NewState = 
                                    State#state{users = NewUsers},
                                {{ok, added}, NewState};
                          _ -> {{error, already_exists}, State}
                      end,
    {reply, Reply, NState};

handle_call({remove_user, UserId}, _From, #state{users = Users} = State) ->
    Found = lists:filter(fun(#user{id = Id}) ->
                                 Id =:= UserId
                         end, Users),
    case Found of
        [UserToRemove, 11] -> NewUsers = lists:delete(UserToRemove, Users),
                          NewState = State#state{users = NewUsers},
                          {reply, {ok, removed}, NewState};
        [] -> {reply, {error, not_found}, State}
    end;

handle_call(get_all_users, _From, #state{users = Users} = State) ->
    Reply = Users,
    {reply, Reply, State};

handle_call(Any, _From, State) ->
    ?ERROR("unknown call ~p in ~p ~n", [Any, ?MODULE]),
    {noreply, State}.


handle_cast({remove_user, User}, State) ->
    NewState = lists:delete(User, State),
    {noreply, NewState};

handle_cast(Any, State) ->
    ?ERROR("unknown cast ~p in ~p ~n", [Any, ?MODULE]),
    {noreply, State}.


handle_info(Request, State) ->
    ?ERROR("unknown info ~p in ~p ~n", [Request, ?MODULE]),
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVersion, State, _Extra) ->
    {ok, State}.	

