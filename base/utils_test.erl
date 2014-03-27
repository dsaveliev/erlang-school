-module(utils_test).

-export([]).

-include_lib("eunit/include/eunit.hrl").

next_day_test() ->
    ?assertEqual({2013, 10, 5}, utils:next_day({2013, 10, 4})),
    ?assertEqual({2013, 9, 30}, utils:next_day({2013, 9, 29})),
    ?assertEqual({2013, 10, 1}, utils:next_day({2013, 9, 30})),
    ok.

prev_day_test() ->
    ?assertEqual({2013, 10, 5}, utils:prev_day({2013, 10, 6})),
    ?assertEqual({2013, 9, 30}, utils:prev_day({2013, 10, 1})),
    ?assertEqual({2013, 12, 31}, utils:prev_day({2014, 1, 1})),
    ok.

add_days_test() ->
    ?assertEqual({2013, 10, 15}, utils:add_days({2013, 10, 5}, 10)),
    ?assertEqual({2012, 3, 1}, utils:add_days({2012, 2, 20}, 10)),
    ?assertEqual({2013, 3, 2}, utils:add_days({2013, 2, 20}, 10)),
    ok.

    
