## Конвертация record -> proplist -> record. Часть 1-я.

Если вы разрабатываете веб-приложение, то наверняка используете JSON
для обмена данными с клиентом. Вполне может быть, что JSON вы
используете, даже если ваш проект не веб :) Между тем, в Erlang
ествественный способ хранения и передачи данных -- это record. Стало
быть, вам нужно будет конвертировать данные в обе стороны между record
и JSON. Давайте посмотрим, что тут можно сделать, и как с этим жить.

Опытным Erlang-разработчикам покажется, что я пишу слишком упрощенно,
и слишком долго подхожу к конечному результату. Это не случайно, ибо я
не просто пишу пост, а готовлю материал для обучения начинающих
разработчиков. Конечная цель -- подготовить хороший учебный курс по
Erlang. И данный пост -- один из шагов к этой цели.

Итак, допустим, у нас в проекте есть такие вот records:

    -record(user, {id = 0 :: integer(),
                   name = <<>> :: binary(),
                   avatar = <<>> :: binary(),
                   level = 0 :: integer()}).

    -record(comment, {id = 0 :: integer(),
                      user_id = 0 :: integer(),
                      text = <<>> :: binary()}).


И мы хотим превратить их в JSON объекты, чтобы передать клиенту. Берем
какой-нибудь JSON-парсер, например
[jsonx](https://github.com/iskra/jsonx), и выясняется, что record он
не принимает, а ему нужно передать proplist вот такого вида:

    [{<<"id">>, 6},
     {<<"name">>, <<"Bob">>},
     {<<"avatar">>, <<>>},
     {<<"level">>, 5}]


## Шаг 1-й

Ну ладно. Давайте мы пойдем самым простым путем, и просто напишем пару
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
    1> c(main0).
    {ok,main0}
    2> main0:test().
    ok


[полный исходный код здесь](https://github.com/yzh44yzh/erlang-school/blob/master/rec2plist/src/main0.erl)

Этот вариант хорош тем, что абсолютно понятен кому угодно, кто знаком с Erlang. Но это хорошо, когда у нас штук 5 разных объектов. Если их уже десяток, то писать такой код немного напрягает. А если пару десятков, то уже и не хочется, а хочется как-то обобщить. По идее, такой код легко поддается обощению. Поэтому идем дальше.


## Шаг 2-й

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

На самом деле код будет чуть сложнее, потому что Fields у нас -- список атомов, а для JSON парсера нужно, чтобы ключи были binary. Поэтому:

    Fields = lists:map(fun(Field) ->
                               list_to_binary(atom_to_list(Field))
                       end, get_fields(user)),
    [_ | Values] = tuple_to_list(Item),
    lists:zip(Fields, Values).


Но беда в том, что никакой такой функции get_fields/1, которая отдавала бы список полей для любого record, имеющегося в проекте, нет. Хотя мы, конечно, можем сами ее написать. Придется предусмотреть клоз для каждого record, который мы захотим конвертировать.

Немного поможет в этом деле псевдофункция

    record_info(fields, Record) -> [Field]
    
о который вы можете прочитать в [документации по records](http://www.erlang.org/doc/reference_manual/records.html).

Беда с ней в том, что это не функция, а магический костыль, работающий только на этапе компиляции. И чтобы его использовать, все равно нужно написать в своем коде

    get_fields(user) -> record_info(fields, user);
    get_fields(comment) -> record_info(fields, comment).

отдельный клоз для каждого record. Но это чуть проще, чем самому перечислять все поля, рискуя где-нибудь ошибиться.


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
    1> c(main1).
    {ok,main1}
    2> main0:test().
    ok


[полный исходный код здесь](https://github.com/yzh44yzh/erlang-school/blob/master/rec2plist/src/main1.erl)

Этот вариант лучше первого, ибо теперь для каждого record нужно написать всего лишь одну магическую строку, а не полную функцию. Но по-хорошему, и от этого нужно избавится. И мы избавимся, но позже :) А сейчас обратим внимание на другую проблему.

Что, если record у нас вложены друг в друга? Вот так:

    -record(comment, {id = 0 :: integer(),
                      user :: #user{},
                      text = <<>> :: binary()}).

Тогда, конвертируя #comment{} мы получим на выходе

    [{<<"id">>, 7}, {<<"user">>, #user{}}, {<<"text">>, <<"hello">>}],

и JSON парсер на нас очень обидится. С этим надо что-то делать :)

