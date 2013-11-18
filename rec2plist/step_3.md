# Шаг 3-й, учитываем вложенные рекорды.

Допустим, мы имеем вложенные рекорды:

    -record(user, {id = 0 :: integer(),
                   name = <<>> :: binary(),
                   avatar = <<>> :: binary(),
                   level = 0 :: integer()}).

    -record(comment, {id = 0 :: integer(),
                      user :: #user{},
                      text = <<>> :: binary()}).


И теперь наши rep2plist/1 и plist2rep/2 должны быть рекурсивными. Но
как нам определить, в каком случае вызывать рекурсию, а в каком просто
использовать значение как есть?

В rep2plist можем допустить, что если значение является кортежем, то
это рекорд, и для него вызвать рекурсию. В остальных случаях не
вызывать:

    rec2plist(Item) ->
        [RecordName | Values] = tuple_to_list(Item),
        Fields = get_fields(RecordName),
        lists:map(fun({Field, Value}) ->
                          BinField = list_to_binary(atom_to_list(Field)),
                          case is_tuple(Value) of
                              true -> {BinField, rec2plist(Value)};
                              false ->{BinField, Value}
                          end
                  end, lists:zip(Fields, Values)).


В plist2rep можно допустить, что если имя поля совпадает с именем
record, то значение будет этим record. Проверять, есть ли у нас record
для данного имени, будем с помощью нашего get_fields/1, куда добавим
клоз, обрабатывающий все неизвестные поля:

    get_fields(user) -> record_info(fields, user);
    get_fields(comment) -> record_info(fields, comment);
    get_fields(_) -> unknown_record.


Тогда plist2rep может выглядеть так:

    plist2rec(RecordName, Props) ->
        Fields = get_fields(RecordName),
        Values = lists:map(fun(Field) ->
                                   BinField = list_to_binary(atom_to_list(Field)),
                                   Val = proplists:get_value(BinField, Props),
                                   case get_fields(Field) of
                                       unknown_record -> Val;
                                       _ -> plist2rec(Field, Val)
                                   end
                           end, Fields),
        list_to_tuple([RecordName | Values]).

Еще нужно будет поправить тесты, чтобы они учитывали новые отношения между records:

    C1 = #comment{id = 7, user = U1, text = <<"hello">>},
    P3 = [{<<"id">>, 7}, {<<"user">>, P1}, {<<"text">>, <<"hello">>}],
    ?assertEqual(P3, rec2plist(C1)),
    ?assertEqual(C1, plist2rec(comment, P3)),
    
    C2 = #comment{user = U2},
    P4 = [{<<"id">>, 0}, {<<"user">>, P2}, {<<"text">>, <<>>}],
    ?assertEqual(P4, rec2plist(C2)),
    ?assertEqual(C2, plist2rec(comment, P4)),

запускаем тесты, и убеждаемся, что все работает.

    yurizhloba ~/p/erlang-school/rec2plist/src $ erl
    Erlang R16B02 (erts-5.10.3) [source] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false]

    Eshell V5.10.3  (abort with ^G)
    1> c(step_3).
    {ok,step_3}
    2> step_3:test().
    ok

[полный исходный код здесь](https://github.com/yzh44yzh/erlang-school/blob/master/rec2plist/src/step_3.erl)

Работает-то оно работает, но является прекрасным образцом говнокода :)
Мало того, что мы сделали два неявных допущения, которые потом
наверняка будут нарушаться, так мы еще не учли вариант, когда значение
поля является списком записей. Например, здесь:

    -record(article, {id = 0 :: integer(),
                      author :: #user{},
                      text = <<>> :: binary(),
                      comments = [] :: [#comment{}]}).

article имеет поле comments, значением которого является список
записей #comment{}. И здесь наша реализация не сработает. Нужна
реализация получше.


