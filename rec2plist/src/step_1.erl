-module(step_1).

-export([rec2plist/1, test/0]).
-include_lib("eunit/include/eunit.hrl").


-record(user, {id = 0 :: integer(),
               name = <<>> :: binary(),
               avatar = <<>> :: binary(),
               level = 0 :: integer()}).


-record(comment, {id = 0 :: integer(),
                  user_id = 0 :: integer(),
                  text = <<>> :: binary()}).


-spec(rec2plist(tuple()) -> list()).
rec2plist(#user{id = Id, name = Name, avatar = Avatar, level = Level}) ->
    [{<<"id">>, Id},
     {<<"name">>, Name},
     {<<"avatar">>, Avatar},
     {<<"level">>, Level}];

rec2plist(#comment{id = Id, user_id = UserId, text = Text}) ->
    [{<<"id">>, Id},
     {<<"user_id">>, UserId},
     {<<"text">>, Text}].



-spec(plist2rec(atom(), list()) -> tuple()).
plist2rec(user, Props) ->
    #user{id = proplists:get_value(<<"id">>, Props),
          name = proplists:get_value(<<"name">>, Props),
          avatar = proplists:get_value(<<"avatar">>, Props),
          level = proplists:get_value(<<"level">>, Props)};

plist2rec(comment, Props) ->
    #comment{id = proplists:get_value(<<"id">>, Props),
             user_id = proplists:get_value(<<"user_id">>, Props),
             text = proplists:get_value(<<"text">>, Props)}.


test() ->
    U1 = #user{id = 5},
    P1 = [{<<"id">>, 5}, {<<"name">>, <<>>}, {<<"avatar">>, <<>>}, {<<"level">>, 0}],
    ?assertEqual(P1, rec2plist(U1)),
    ?assertEqual(U1, plist2rec(user, P1)),

    U2 = #user{id = 6, name = <<"Bob">>, level = 5},
    P2 = [{<<"id">>, 6}, {<<"name">>, <<"Bob">>}, {<<"avatar">>, <<>>}, {<<"level">>, 5}],
    ?assertEqual(P2, rec2plist(U2)),
    ?assertEqual(U2, plist2rec(user, P2)),
    
    C1 = #comment{id = 7, user_id = 5, text = <<"hello">>},
    P3 = [{<<"id">>, 7}, {<<"user_id">>, 5}, {<<"text">>, <<"hello">>}],
    ?assertEqual(P3, rec2plist(C1)),
    ?assertEqual(C1, plist2rec(comment, P3)),
    
    C2 = #comment{},
    P4 = [{<<"id">>, 0}, {<<"user_id">>, 0}, {<<"text">>, <<>>}],
    ?assertEqual(P4, rec2plist(C2)),
    ?assertEqual(C2, plist2rec(comment, P4)),
    
    ok.
