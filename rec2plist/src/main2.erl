-module(main2).

-export([rec2plist/1, test/0]).
-include_lib("eunit/include/eunit.hrl").


-record(user, {id = 0 :: integer(),
               name = <<>> :: binary(),
               avatar = <<>> :: binary(),
               level = 0 :: integer()}).


-record(comment, {id = 0 :: integer(),
                  user :: #user{},
                  text = <<>> :: binary()}).


get_fields(user) -> record_info(fields, user);
get_fields(comment) -> record_info(fields, comment);
get_fields(_) -> unknown_record.
     

-spec(rec2plist(tuple()) -> list()).
rec2plist(Item) ->
    [RecordName | Values] = tuple_to_list(Item),
    case get_fields(RecordName) of
        unknown_record -> Item;
        Fields when length(Fields) =:= length(Values) ->
            lists:map(fun({Field, Value}) ->
                              BinField = list_to_binary(atom_to_list(Field)),
                              case is_tuple(Value) of
                                  true -> {BinField, rec2plist(Value)};
                                  false ->{BinField, Value}
                              end
                      end, lists:zip(Fields, Values))
    end.


plist2rec(RecordName, Props) ->
    Fields = get_fields(RecordName),
    Values = lists:map(fun(Field) ->
                               BinField = list_to_binary(atom_to_list(Field)),
                               Val = proplists:get_value(BinField, Props),
                               case get_fields(Field) of
                                   unknown_record -> Val;
                                   _ -> plist2rec(Field, Val)
                               end
                       end, Fields),
    list_to_tuple([RecordName | Values]).
    
                   
test() ->
    U1 = #user{id = 5},
    P1 = [{<<"id">>, 5}, {<<"name">>, <<>>}, {<<"avatar">>, <<>>}, {<<"level">>, 0}],
    ?assertEqual(P1, rec2plist(U1)),
    ?assertEqual(U1, plist2rec(user, P1)),

    U2 = #user{id = 6, name = <<"Bob">>, level = 5},
    P2 = [{<<"id">>, 6}, {<<"name">>, <<"Bob">>}, {<<"avatar">>, <<>>}, {<<"level">>, 5}],
    ?assertEqual(P2, rec2plist(U2)),
    ?assertEqual(U2, plist2rec(user, P2)),
    
    C1 = #comment{id = 7, user = U1, text = <<"hello">>},
    P3 = [{<<"id">>, 7}, {<<"user">>, P1}, {<<"text">>, <<"hello">>}],
    ?assertEqual(P3, rec2plist(C1)),
    ?assertEqual(C1, plist2rec(comment, P3)),
    
    C2 = #comment{user = U2},
    P4 = [{<<"id">>, 0}, {<<"user">>, P2}, {<<"text">>, <<>>}],
    ?assertEqual(P4, rec2plist(C2)),
    ?assertEqual(C2, plist2rec(comment, P4)),
    
    ok.
