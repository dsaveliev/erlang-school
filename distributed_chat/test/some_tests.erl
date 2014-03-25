-module(some_tests).
-author('Yura Zhloba <yzh44yzh@gmail.com>').

-export([]).
-include("types.hrl").
-include("logger.hrl").
-include_lib("eunit/include/eunit.hrl").


start_test() ->
    ?assert(5 == 5),
    ?assertEqual({ok, 5}, {ok, 5}),
    ?assertMatch({ok, _}, {ok, 5}).

