-module(db_tests).
-include_lib("eunit/include/eunit.hrl").

get_value_test() ->
    db:start_link(test),
    ?assertEqual({error, not_found}, db:get_value(1)),
    ?assertEqual(ok, db:add_value(1,2)),
    ?assertEqual({ok, 2}, db:get_value(1)),
    ?assertEqual({error, not_found}, db:get_value(10)),
    ?assertEqual(ok, db:add_value(10, "Hello")),
    ?assertEqual({ok, "Hello"}, db:get_value(10)),
    ?assertEqual(ok, db:add_value(10, "Hello again")),
    ?assertEqual({ok, "Hello again"}, db:get_value(10)),
    db:stop(),
    ok.


delete_value_test() ->
    db:start_link(test),
    ?assertEqual(not_found, db:delete_value(1)),
    ?assertEqual(ok, db:add_value(1,2)),
    ?assertEqual(ok, db:delete_value(1)),
    ?assertEqual(not_found, db:delete_value(1)),
    ?assertEqual(ok, db:add_value(3, {1,2,3})),
    ?assertEqual(ok, db:add_value(4, [1,2,3])),
    ?assertEqual(ok, db:delete_value(3)),
    ?assertEqual(not_found, db:delete_value(3)),
    ?assertEqual(ok, db:delete_value(4)),
    ?assertEqual(not_found, db:delete_value(4)),
    db:stop(),
    ok.
