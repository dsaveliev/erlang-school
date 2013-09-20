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
    ?assertEqual({error, name_exists}, kvs:create("Bill", "1")),
    ?assertEqual({error, name_exists}, kvs:create("Bob", "2")),
    ?assertEqual({error, name_exists}, kvs:create("Frank", "2")),
    ?assertEqual({error, name_exists}, kvs:create("Fish", "2")),
    ?assertEqual(ok, kvs:create("Duck", "5566")),
    ?assertEqual({error, name_exists}, kvs:create("Duck", "2")),
    kvs:clear().


read_test() ->
    ?assertEqual({error, no_name}, kvs:read("Bob")),
    ?assertEqual(ok, kvs:create("Bob", "1122")),
    ?assertEqual({ok, "1122"}, kvs:read("Bob")),
    ?assertEqual({error, no_name}, kvs:read("Bill")),
    ?assertEqual(ok, kvs:create("Bill", "2233")),
    ?assertEqual({ok, "2233"}, kvs:read("Bill")),
    ?assertEqual({error, no_name}, kvs:read("Frank")),
    ?assertEqual(ok, kvs:create("Frank", "3344")),
    ?assertEqual({ok, "3344"}, kvs:read("Frank")),
    ?assertEqual(ok, kvs:create("Duck", "4455")),
    ?assertEqual({ok, "4455"}, kvs:read("Duck")),
    ?assertEqual(ok, kvs:create("Fool", "5566")),
    ?assertEqual({ok, "5566"}, kvs:read("Fool")),
    kvs:clear().


update_test() ->
    ?assertEqual({error, no_name}, kvs:update("Bob", "4455")),
    ?assertEqual(ok, kvs:create("Bob", "1122")),
    ?assertEqual(ok, kvs:create("Bill", "2233")),
    ?assertEqual({ok, "1122"}, kvs:read("Bob")),
    ?assertEqual(ok, kvs:update("Bob", "4455")),
    ?assertEqual(ok, kvs:update("Bill", "7788")),
    ?assertEqual({ok, "4455"}, kvs:read("Bob")),
    ?assertEqual({ok, "7788"}, kvs:read("Bill")),
    ?assertEqual(ok, kvs:create("Frank", "aa1122")),
    ?assertEqual({ok, "aa1122"}, kvs:read("Frank")),
    ?assertEqual(ok, kvs:update("Frank", "aa4455")),
    ?assertEqual({ok, "aa4455"}, kvs:read("Frank")),
    ?assertEqual({error, no_name}, kvs:update("Fish", "3333")),
    ?assertEqual(ok, kvs:create("Fish", "1111")),
    ?assertEqual({ok, "1111"}, kvs:read("Fish")),
    ?assertEqual(ok, kvs:update("Fish", "3333")),
    ?assertEqual({ok, "3333"}, kvs:read("Fish")),
    kvs:clear().


delete_test() ->
    ?assertEqual({error, no_name}, kvs:delete("Bill")),
    ?assertEqual({error, no_name}, kvs:delete("Bob")),
    ?assertEqual(ok, kvs:create("Bob", "1")),
    ?assertEqual(ok, kvs:create("Bill", "2")),
    ?assertEqual(ok, kvs:delete("Bill")),
    ?assertEqual(ok, kvs:delete("Bob")),
    ?assertEqual({error, no_name}, kvs:delete("Bill")),
    ?assertEqual(ok, kvs:create("Frank", "1")),
    ?assertEqual(ok, kvs:create("Fish", "2")),
    ?assertEqual(ok, kvs:delete("Fish")),
    ?assertEqual(ok, kvs:delete("Frank")),
    ?assertEqual({error, no_name}, kvs:delete("Frank")),
    kvs:clear().
