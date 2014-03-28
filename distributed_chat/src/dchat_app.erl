-module(dchat_app).
-author('Yura Zhloba <yzh44yzh@gmail.com>').

-behaviour(application).
-export([start/0, start/1, start/2, stop/1]).
-include("logger.hrl").


%%% module API

start() -> start(['8080']).

start([Port0]) ->
    lager:start(),
    Port = list_to_integer(atom_to_list(Port0)),

    ok = application:start(crypto),
    ok = application:start(ranch),
    ok = application:start(cowlib),
    ok = application:start(cowboy),

    Routes = cowboy_router:compile(routes()),
    cowboy:start_http(http, 100,
                      [{port, Port}],
                      [{env, [{dispatch, Routes}]}]),
    ?INFO("cowboy started at port ~p", [Port]),

    ok = application:start(dchat),
    ?INFO("dchat app started"),

    connect_nodes(),
    ok.


%%% application API

start(_StartType, _StartArgs) ->
    dchat_sup:start_link().


stop(_State) ->
    ok = application:stop(cowboy),
    ok = application:stop(cowlib),
    ok = application:stop(ranch),
    ok = application:stop(crypto),
    ok.


%%% inner functions

routes()->
    [{'_',
      [
       {"/api/chat/", bullet_handler, [{handler, chat_handler}]},
       {"/", cowboy_static, {file, "priv/static/index.html", [{mimetypes, cow_mimetypes, all}]}},
       {"/[...]", cowboy_static, {dir, "priv/static", [{mimetypes, cow_mimetypes, all}]}},
       {'_', not_found_handler, []}
      ]}].


connect_nodes() ->
    {ok, Nodes0} = application:get_env(dchat, nodes),
    Nodes = lists:delete(node(), Nodes0),
    ?INFO("try to connect nodes ~p", [Nodes]),
    Results = lists:map(fun(Node) ->
                                net_kernel:connect_node(Node)
                        end, Nodes),
    case lists:member(true, Results) of
        true -> ?INFO("has connections ~p", [nodes()]),
                [Node | _] = nodes(),
                Online = rpc:call(Node, dchat, get_online, [], 3000),
                ?INFO("Online: ~p", [Online]),
                History = rpc:call(Node, dchat, get_history, [], 3000),
                ?INFO("History: ~p", [History]),
                case {Online, History} of
                    {{badrpc, _}, _} -> do_nothing;
                    {_, {badrpc, _}} -> do_nothing;
                    _ -> dchat:sync(Online, History)
                end,
                ok;
        false -> ?INFO("no connections")
    end,
    ok.
