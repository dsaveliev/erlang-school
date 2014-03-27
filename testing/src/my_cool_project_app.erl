-module(my_cool_project_app).
-author('Yura Zhloba <yzh44yzh@gmail.com>').

-behaviour(application).
-export([start/0, start/2, stop/1]).
-include("logger.hrl").

start() ->
    lager:start(),
    application:start(my_cool_project),
    ok.


start(_StartType, _StartArgs) ->
    my_cool_project_sup:start_link().

    
stop(_State) ->
    lager:stop(),
    ok.
