# Шаг 1-й, конвернируем вручную.

Давайте мы пойдем самым простым путем, и просто напишем пару
функций, которые преобразуют данные как нам нужно:

    rec2plist(#user{id = Id, name = Name, avatar = Avatar, level = Level}) ->
        [{<<"id">>, Id},
         {<<"name">>, Name},
         {<<"avatar">>, Avatar},
         {<<"level">>, Level}];

    rec2plist(#comment{id = Id, user_id = UserId, text = Text}) ->
        [{<<"id">>, Id},
         {<<"user_id">>, UserId},
         {<<"text">>, Text}].


    plist2rec(user, Props) ->
        #user{id = proplists:get_value(<<"id">>, Props),
              name = proplists:get_value(<<"name">>, Props),
              avatar = proplists:get_value(<<"avatar">>, Props),
              level = proplists:get_value(<<"level">>, Props)};

    plist2rec(comment, Props) ->
        #comment{id = proplists:get_value(<<"id">>, Props),
                 user_id = proplists:get_value(<<"user_id">>, Props),
                 text = proplists:get_value(<<"text">>, Props)}.


Ну что ж, это было просто. Добавим тесты, чтобы убедиться, что все работает как надо.

    test() ->
        U1 = #user{id = 5},
        P1 = [{<<"id">>, 5}, {<<"name">>, <<>>}, {<<"avatar">>, <<>>}, {<<"level">>, 0}],
        ?assertEqual(P1, rec2plist(U1)),
        ?assertEqual(U1, plist2rec(user, P1)),

        U2 = #user{id = 6, name = <<"Bob">>, level = 5},
        P2 = [{<<"id">>, 6}, {<<"name">>, <<"Bob">>}, {<<"avatar">>, <<>>}, {<<"level">>, 5}],
        ?assertEqual(P2, rec2plist(U2)),
        ?assertEqual(U2, plist2rec(user, P2)),
    
        C1 = #comment{id = 7, user_id = 5, text = <<"hello">>},
        P3 = [{<<"id">>, 7}, {<<"user_id">>, 5}, {<<"text">>, <<"hello">>}],
        ?assertEqual(P3, rec2plist(C1)),
        ?assertEqual(C1, plist2rec(comment, P3)),
    
        C2 = #comment{},
        P4 = [{<<"id">>, 0}, {<<"user_id">>, 0}, {<<"text">>, <<>>}],
        ?assertEqual(P4, rec2plist(C2)),
        ?assertEqual(C2, plist2rec(comment, P4)),
    
        ok.

Запускаем, и убеждаемся, что действительно, все ок.

    yurizhloba ~/p/erlang-school/rec2plist/src $ erl
    Erlang R16B02 (erts-5.10.3) [source] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false]

    Eshell V5.10.3  (abort with ^G)
    1> c(step_1).
    {ok,step_1}
    2> step_1:test().
    ok


[полный исходный код здесь](https://github.com/yzh44yzh/erlang-school/blob/master/rec2plist/src/step_1.erl)

Этот вариант хорош тем, что абсолютно понятен кому угодно, кто знаком
с Erlang. Но это хорошо, когда у нас штук 5 разных объектов. Если их
уже десяток, то писать такой код немного напрягает. А если пару
десятков, то уже и не хочется, а хочется как-то обобщить. По идее,
такой код легко поддается обощению. Поэтому идем дальше.

