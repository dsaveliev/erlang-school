-module(kvs_http_handler).
-author('Yura Zhloba <yzh44yzh@gmail.com>').

-behaviour(cowboy_http_handler).
 
-export([init/3, handle/2, terminate/3]).

 
%%% module API

init({tcp, http}, Req, _Opts) ->
    {ok, Req, undefined_state}.

 
handle(Req, State) ->
    {Url, _} = cowboy_req:url(Req),
    {HostUrl, _} = cowboy_req:host_url(Req),
    Params = lists:subtract(binary_to_list(Url), binary_to_list(HostUrl)),
    Res = case parse_query(Params) of
              {create, Key, Value} -> kvs:create(Key, Value);
              {read, Key} -> kvs:read(Key);
              {update, Key, Value} -> kvs:update(Key, Value);
              {delete, Key} -> kvs:delete(Key);
              error -> "invalid query"
          end,
    Reply = io_lib:format("~p", [Res]),
    {ok, Req2} = cowboy_req:reply(200, [], list_to_binary(Reply), Req),
    {ok, Req2, State}.

 
terminate(_Reason, _Req, _State) ->
    ok.


parse_query(Data) ->
    case string:tokens(Data, "/") of
        ["create" | Tokens] when length(Tokens) > 1 ->
            [Key | Value] = Tokens,
            {create, Key, string:join(Value, " ")};
        ["read" | Tokens] when length(Tokens) > 0 ->
            [Key | _] = Tokens,
            {read, Key};
        ["update" | Tokens] when length(Tokens) > 1 ->
            [Key | Value] = Tokens,
            {update, Key, string:join(Value, " ")};
        ["delete" | Tokens] when length(Tokens) > 0 ->
            [Key | _] = Tokens,
            {delete, Key};
        _ -> error
    end.
