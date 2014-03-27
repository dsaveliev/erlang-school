-module(my_server_test).
-include_lib("eunit/include/eunit.hrl").

my_server_test() ->
    my_server:start_link(),
    my_server:add_stat(create, 5),
    my_server:add_stat(create, 10),
    my_server:add_stat(create, 15),
    my_server:add_stat(create, 20),
    my_server:add_stat(create, 25),
    ?assertEqual({5, 25, 75/5}, my_server:get_stat(create)),
    my_server:add_stat(create, 30),
    ?assertEqual({5, 30, 105/6}, my_server:get_stat(create)),
    ?assertEqual({error, not_found}, my_server:get_stat(read)),
    my_server:stop().
