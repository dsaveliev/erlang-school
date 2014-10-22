-module(calendar_lib_test).
-include_lib("eunit/include/eunit.hrl").

prev_date_test() ->
    ?assertEqual({2013, 10, 12}, calendar_lib:prev_date({2013, 10, 13})),
    ?assertEqual({2013, 9, 30}, calendar_lib:prev_date({2013, 10, 1})),
    ?assertEqual({2013, 2, 28}, calendar_lib:prev_date({2013, 3, 1})),
    ?assertEqual({2012, 2, 29}, calendar_lib:prev_date({2012, 3, 1})),
    ?assertEqual({2012, 12, 31}, calendar_lib:prev_date({2013, 1, 1})),
    ok.

next_date_test() ->
    ?assertEqual({2013, 10, 14}, calendar_lib:next_date({2013, 10, 13})),
    ?assertEqual({2013, 10, 1}, calendar_lib:next_date({2013, 9, 30})),
    ?assertEqual({2013, 3, 1}, calendar_lib:next_date({2013, 2, 28})),
    ?assertEqual({2012, 2, 29}, calendar_lib:next_date({2012, 2, 28})),
    ?assertEqual({2014, 1, 1}, calendar_lib:next_date({2013, 12, 31})),
    ok.


add_date_test() ->
    ?assertEqual({2013, 10, 19}, calendar_lib:add_days({2013, 10, 9}, 10)),
    ?assertEqual({2012, 3, 1}, calendar_lib:add_days({2012, 2, 20}, 10)),
    ok.

date_range_test() ->
    ?assertEqual([{2013, 10, 8}, {2013, 10, 9}, {2013, 10, 10}],
                 calendar_lib:date_range({2013, 10, 8}, {2013, 10, 10})),
    ?assertEqual([{2013, 10, 8}],
                 calendar_lib:date_range({2013, 10, 8}, {2013, 10, 8})),
    ?assertEqual(error, calendar_lib:date_range({2012,1,1}, {2010,1,1})),
    ok.
