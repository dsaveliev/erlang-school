-module(kvs_test).
-author('Yura Zhloba <yzh44yzh@gmail.com>').

-include("kvs_types.hrl").
-include_lib("eunit/include/eunit.hrl").

create_test() ->
    kvs:start_link([]),
    kvs:clear(),
    ?assertEqual(ok, kvs:create("Bob", "1122")),
    ?assertEqual(ok, kvs:create("Bill", "2233")),
    ?assertEqual(ok, kvs:create("Frank", "3344")),
    ?assertEqual(ok, kvs:create("Fish", "4455")),
    ?assertEqual(ok, kvs:create("Bill", "1")),
    ?assertEqual(ok, kvs:create("Bob", "2")),
    ?assertEqual(ok, kvs:create("Frank", "2")),
    ?assertEqual(ok, kvs:create("Fish", "2")),
    ?assertEqual(ok, kvs:create("Duck", "5566")),
    ?assertEqual(ok, kvs:create("Duck", "2")),
    kvs:clear().


read_test() ->
    ?assertEqual({error, no_value}, kvs:read("Bob")),
    ?assertEqual(ok, kvs:create("Bob", "1122")),
    ?assertMatch({ok, [{_, "1122"}]}, kvs:read("Bob")),
    ?assertEqual(ok, kvs:create("Bob", "2233")),
    ?assertMatch({ok, [{_, "2233"}, {_, "1122"}]}, kvs:read("Bob")),

    ?assertEqual({error, no_value}, kvs:read("Bill")),
    ?assertEqual(ok, kvs:create("Bill", "2233")),
    ?assertMatch({ok, [{_, "2233"}]}, kvs:read("Bill")),

    ?assertEqual({error, no_value}, kvs:read("Frank")),
    ?assertEqual(ok, kvs:create("Frank", "1122")),
    ?assertEqual(ok, kvs:create("Frank", "2233")),
    ?assertEqual(ok, kvs:create("Frank", "3344")),
    ?assertEqual(ok, kvs:create("Frank", "4455")),
    ?assertEqual(ok, kvs:create("Frank", "5566")),
    ?assertMatch({ok, [{_, "5566"}, {_, "4455"}, {_, "3344"}]}, kvs:read("Frank")),
    kvs:clear().


update_test() ->
    ?assertEqual({error, no_value}, kvs:update("Bob", "4455")),
    ?assertEqual(ok, kvs:create("Bob", "1122")),
    ?assertEqual(ok, kvs:create("Bob", "2233")),
    ?assertMatch({ok, [{_, "2233"}, {_, "1122"}]}, kvs:read("Bob")),
    ?assertEqual(ok, kvs:update("Bob", "4455")),
    ?assertMatch({ok, [{_, "4455"}, {_, "1122"}]}, kvs:read("Bob")),

    ?assertEqual({error, no_value}, kvs:update("Bill", "0000")),
    ?assertEqual(ok, kvs:create("Bill", "1111")),
    ?assertEqual(ok, kvs:create("Bill", "2222")),
    ?assertMatch({ok, [{_, "2222"}, {_, "1111"}]}, kvs:read("Bill")),
    ?assertEqual(ok, kvs:update("Bill", "3333")),
    ?assertMatch({ok, [{_, "3333"}, {_, "1111"}]}, kvs:read("Bill")),

    ?assertEqual(ok, kvs:update("Bob", "1111")),
    ?assertMatch({ok, [{_, "1111"}, {_, "1122"}]}, kvs:read("Bob")),
    kvs:clear().


delete_test() ->
    ?assertEqual({error, no_value}, kvs:delete("Bill")),
    ?assertEqual({error, no_value}, kvs:delete("Bob")),
    ?assertEqual(ok, kvs:create("Bob", "1")),
    ?assertEqual(ok, kvs:create("Bill", "2")),
    ?assertEqual(ok, kvs:delete("Bill")),
    ?assertEqual(ok, kvs:delete("Bob")),
    ?assertEqual({error, no_value}, kvs:delete("Bill")),
    ?assertEqual(ok, kvs:create("Frank", "1")),
    ?assertEqual(ok, kvs:create("Fish", "2")),
    ?assertEqual(ok, kvs:delete("Fish")),
    ?assertEqual(ok, kvs:delete("Frank")),
    ?assertEqual({error, no_value}, kvs:delete("Frank")),
    kvs:clear().
