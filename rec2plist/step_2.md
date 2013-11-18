# Шаг 2-й, простая автоматизация.

Итак, record суть обычный tuple с синтаксическим сахором. Причем сахар
у нас есть только на этапе компиляции, а в рантайме нету. Стало быть,
наш

    User = #user{id = 6, name = <<"Bob">>, level = 5}

это не что иное, как

    User = {user, 6, <<"Bob">>, <<>>, 5}

Проблема в том, что в рантайме у нас нет информации о названиях полей record.

Если бы такая информация была, например, если бы существовала функция, типа:

    get_fields(user) -> [id, name, avatar, level];
    get_fields(comment) -> [id, user_id, text].

то конвертировать было бы просто:

    Fields = get_fields(user),
    [_ | Values] = tuple_to_list(user),
    lists:zip(Fields, Values).

и все дела.

На самом деле код будет чуть сложнее, потому что Fields у нас --
список атомов, а для JSON парсера нужно, чтобы ключи были
binary. Поэтому:

    Fields = lists:map(fun(Field) ->
                               list_to_binary(atom_to_list(Field))
                       end, get_fields(user)),
    [_ | Values] = tuple_to_list(Item),
    lists:zip(Fields, Values).


Но беда в том, что никакой такой функции get_fields/1, которая
отдавала бы список полей для любого record, имеющегося в проекте,
нет. Хотя мы, конечно, можем сами ее написать. Придется предусмотреть
клоз для каждого record, который мы захотим конвертировать.

Немного поможет в этом деле псевдофункция

    record_info(fields, Record) -> [Field]
    
о который вы можете прочитать в [документации по records](http://www.erlang.org/doc/reference_manual/records.html).

Беда с ней в том, что это не функция, а магический костыль, работающий
только на этапе компиляции. И чтобы его использовать, все равно нужно
написать в своем коде

    get_fields(user) -> record_info(fields, user);
    get_fields(comment) -> record_info(fields, comment).

отдельный клоз для каждого record. Но это чуть проще, чем самому
перечислять все поля, рискуя где-нибудь ошибиться.


В итоге получаем такой код:

    get_fields(user) -> record_info(fields, user);
    get_fields(comment) -> record_info(fields, comment).
     

    -spec(rec2plist(tuple()) -> list()).
    rec2plist(Item) ->
        [RecordName | Values] = tuple_to_list(Item),
        Fields = lists:map(fun(Field) ->
                                   list_to_binary(atom_to_list(Field))
                           end, get_fields(RecordName)),
        lists:zip(Fields, Values).


    -spec(plist2rec(atom(), list()) -> tuple()).
    plist2rec(RecordName, Props) ->
        Fields = get_fields(RecordName),
        Values = lists:map(fun(Field) ->
                                   BinField = list_to_binary(atom_to_list(Field)),
                                   proplists:get_value(BinField, Props)
                           end, Fields),
        list_to_tuple([RecordName | Values]).

запускаем на нем те же тесты, и убеждаемся, что все работает.

    yurizhloba ~/p/erlang-school/rec2plist/src $ erl
    Erlang R16B02 (erts-5.10.3) [source] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false]

    Eshell V5.10.3  (abort with ^G)
    1> c(step_2).
    {ok,step_2}
    2> step_2:test().
    ok


[полный исходный код здесь](https://github.com/yzh44yzh/erlang-school/blob/master/rec2plist/src/step_2.erl)

Этот вариант лучше первого, ибо теперь для каждого record нужно
написать всего лишь одну магическую строку, а не полную функцию. Но
по-хорошему, и от этого нужно избавится. И мы избавимся, но позже :) А
сейчас обратим внимание на другую проблему.

Что, если record у нас вложены друг в друга? Вот так:

    -record(comment, {id = 0 :: integer(),
                      user :: #user{},
                      text = <<>> :: binary()}).

Тогда, конвертируя #comment{} мы получим на выходе

    [{<<"id">>, 7}, {<<"user">>, #user{}}, {<<"text">>, <<"hello">>}],

и JSON парсер на нас очень обидится. С этим надо что-то делать :)

