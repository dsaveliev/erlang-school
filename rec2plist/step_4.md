# Шаг 4-й, учитываем вложенные списки рекордов.

Итак, возьмем более сложную структуру записей:

    -record(user, {id = 0 :: integer(),
                   name = <<>> :: binary(),
                   avatar = <<>> :: binary(),
                   level = 0 :: integer()}).

    -record(comment, {id = 0 :: integer(),
                      user :: #user{},
                      text = <<>> :: binary()}).

    -record(article, {id = 0 :: integer(),
                      author :: #user{},
                      text = <<>> :: binary(),
                      comments = [] :: [#comment{}]}).


Здесь #comment{} имеет вложенную запись #user{}, и #article{} тоже
имеет вложенную запись #user{}, причем поле называется по-другому, и
еще #article{} имеет список #comment{}.

Теперь нам мало иметь get_fields/1, возвращающую названия полей для
record. Еще нужно откуда-то брать информацию о типах значений для
данных полей. Что ж, добавим функцю, возвращающую нам такую
информацию:

    get_relation(comment, user) -> {record, user};
    get_relation(article, author) -> {record, user};
    get_relation(article, comments) -> {list, comment};
    get_relation(_, _) -> simple_value.

Теперь мы знаем, что поле user в записи #comment является записью
\#user{}, а поле comments в записи #article{} является списоком записей
\#comment{}. И с этой инфой мы можем правильно реализовать rec2plist/1
и plist2rec/2:

    rec2plist(Item) ->
        [RecordName | Values] = tuple_to_list(Item),
        Fields = get_fields(RecordName),
        lists:map(fun({Field, Value}) ->
                          BinField = list_to_binary(atom_to_list(Field)),
                          case get_relation(RecordName, Field) of
                              {record, _} -> {BinField, rec2plist(Value)};
                              {list, _} -> {BinField, lists:map(fun(Val) -> rec2plist(Val) end, Value)};
                              simple_value -> {BinField, Value}
                          end
                  end, lists:zip(Fields, Values)).

    plist2rec(RecordName, Props) ->
        Fields = get_fields(RecordName),
        Values = lists:map(fun(Field) ->
                                   BinField = list_to_binary(atom_to_list(Field)),
                                   Value = proplists:get_value(BinField, Props),
                                   case get_relation(RecordName, Field) of
                                       {record, Name} -> plist2rec(Name, Value);
                                       {list, Name} -> lists:map(fun(Val) -> plist2rec(Name, Val) end, Value);
                                       simple_value -> Value
                                   end
                           end, Fields),
        list_to_tuple([RecordName | Values]).

Поправим наши тесты:

    A1 = #article{id = 4, author = U2, text = <<"Hello">>},
    P5 = [{<<"id">>, 4}, {<<"author">>, P2}, {<<"text">>, <<"Hello">>}, {<<"comments">>, []}],
    ?assertEqual(P5, rec2plist(A1)),
    ?assertEqual(A1, plist2rec(article, P5)),

    A2 = #article{id = 5, author = U1, text = <<"Hello again">>, comments = [C1, C2]},
    P6 = [{<<"id">>, 5}, {<<"author">>, P1}, {<<"text">>, <<"Hello again">>}, {<<"comments">>, [P3, P4]}],
    ?assertEqual(P6, rec2plist(A2)),
    ?assertEqual(A2, plist2rec(article, P6)),

и убеждаемся, что все работает.

    yurizhloba ~/p/erlang-school/rec2plist/src $ erl
    Erlang R16B02 (erts-5.10.3) [source] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false]

    Eshell V5.10.3  (abort with ^G)
    1> c(step_4).
    {ok,step_4}
    2> step_4:test().
    ok

[полный исходный код здесь](https://github.com/yzh44yzh/erlang-school/blob/master/rec2plist/src/step_4.erl)

Вот теперь наша реализация неплоха, с этим уже можно
жить. Однако можно сделать и лучше.

Сейчас нам нужно писать get_fields/1 для каждого record, и еще
get\_relation/2 для каждого поля, в котором значение является
record. При том, что вся нужная инфа -- структура records и отношения
между ними, у нас уже есть в определениях -record. Но в рантайме мы не
имеем доступа к этой инфе, поэтому нам приходится дублировать ее еще
раз, уже в другом виде, пригодном для рантайма.

Такое дублирование зло, ибо это нарушение принципа SPOT (Single Point
of Truth). Это чревато тем, что внося изменения в определения
-records, мы можем забыть обновить наши get_relation/2. Да и вообще
писать руками лишний код -- плохо.

А хорошо, в данном случае, этот код сгененировать автоматически :)
