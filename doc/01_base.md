## Основы Erlang 

### Работа в erlang-shell. 
запуск erl
выход C-G q
help()

b()        -- display all variable bindings
f()        -- forget all variable bindings
f(X)       -- forget the binding of variable X
c(File)    -- compile and load code in <File>
l(Module)  -- load or reload module
flush()    -- flush any messages sent to the shell
q()        -- quit - shorthand for init:stop()

i()        -- information about the system
i(X,Y,Z)   -- information about pid <X,Y,Z>
m()        -- which modules are loaded
m(Mod)     -- information about module <Mod>
memory()   -- memory allocation information
regs()     -- information about registered processes

C-G User switch command
 --> h
  c [nn]            - connect to job
  i [nn]            - interrupt job
  k [nn]            - kill job
  j                 - list all jobs
  s [shell]         - start local shell
  r [node [shell]]  - start remote shell
  q        - quit erlang
  ? | h             - this message


### atom, tuple, record
atom -- константное значение
'Atom in single quote'

Аналог в Java -- enum
enum Season { WINTER, SPRING, SUMMER, AUTUMN }

Аналог в C -- define
define WIDTH       80

делают код более понятным:
{ok, Value}
{error, Reason}
{user, "Bob", 22}
-type(my_bool() :: true | false | undefined).

хранятся в таблице атомов, занимают 4 или 8 байт, эта память не освобождается

tuple, кортеж -- обычная структура данных для функциональных языков, но почти не встречается в языках императивных. Подобно списку, это набор значений, доступных по их позиции. Но кортеж имеет фиксированную длинну, не больше и не меньше. Такая структура может показаться странной, но она очень удобна в pattern matching. И поэтому используется повсемесно, так же как и сам pattern matching.

В императивных языках обычно используют словари (в Java, наример, варианты Map), там где в функциональных используют кортежи. Кортежи проигрывают в ясности значений, зато сильно выигрывают в лаконичности кода. Ну а ясность значений можно подчеркнуть там, где это необходимо

Для этого первым элементом обычно ставят атом, указывающий на суть данных:
{point, 10, 15} 
{rect, {point, 10, 10}, {point, 20, 20}}
{user, 22, "Bob"}
{direction, up}
{error, "user not found"}

Но все-таки кортежи сами по себе хороши для небольших объектов, на 2-4 поля. Если нужно больше полей, то тут на помощь приходит record


record, запись. Представляет собой синтаксический сахар, добавленный в язык позже. На этапе компиляции превращается в обычный кортеж и имена полей теряются. Зато принципы работы с ними вполне аналогичны работе со словорями в других языках.

-record(user, {id, name, age}). % {user, undefined, undefined, undefined}
-record(user, {id = 0, name = "", age = 0}). % {user, 0, "", 0}
-record(user, {
    id = 0 :: user_id(),
    name = "" :: string(),
    age = 0 :: age()
    }).
    
User = #user{}
User = #user{id = 5}
User = #user{id = 5, name = "Bob", age = 24}
UserId = User#user.id
UserName = User#user.name
User2 = User#user{id = 7}.

Функции для работы с записями из консоли:
rd(R,D)    -- define a record
rl()       -- display all record information
rr(File)   -- read record information from File (wildcards allowed)

2> rd(user, {id, name, age}).
user
3> rl().
-record(user,{id,name,age}).
ok
4> {user, 5, "Bob", 23}.
\#user{id = 5,name = "Bob",age = 23}
5> Bob = #user{id = 6, name = "Bob"}.
\#user{id = 6,name = "Bob",age = undefined}
6> Bill = Bob#user{name = "Bill"}.
\#user{id = 6,name = "Bill",age = undefined}
7> Bob#user.id.
6
8> rr("kvs.erl").
[state]
9> rl().
-record(state,{data\_file,items = []}).
-record(user,{id,name,age}).
ok
10> State = #state{}.
\#state{data\_file = undefined,items = []}
11> State2 = #state{items = [1,2,3]}.
\#state{data_file = undefined,items = [1,2,3]}
12> 


### pattern matching

Сопоставление с образцом, корявый термин, но очень мощная синтаксическая конструкция. Используется повсеместно для кучи разных целей:
- присваивание значения
- выборка значения из кортежа или списка
- проверка условия
- конструкции условного перехода

Используется:
- клозы функций
- case
- receive
- try..catch

Настолько повсеместно, что в Erlang даже нет операции присваивания, а оператор = выполняет pattern matching ). То есть в коде:
X = 10
происходит матчинг значения 10 в неопределенную переменную X.


### case, if, guards

case find_user(UserId) of
    {ok, User} -> do_something(User), % код с побочными эффектами -- это нормально для Erlang
                  true;
    {error, not_found} -> false
end

case find_user(UserId) of
    {ok, #user{age = Age} = User} when Age > 18 -> do_something(User), ok;
    {error, not_found} -> false
end

if
    User#user.age > 18 -> do_something(User), true;
    User#user.age > 21 -> do_other(User), true;
    true -> false
end

if -- это как если бы из case убрать вычисление значение и матчинг с образцами, но оставить только гарды


### function, close, module, анонимные функции

чего тут писать, тут кодить надо
base/utils.erl


### Структуры данных: list, array, proplist, io_list, dict, ets

list -- рабочая лошадка всех функциональных языков. Основная структура данных. А в некоторых языках (в Lisp) -- вообще единственная структура данных. Представляет собой однонаправленный связанный список.

Стоимость операций:
- добавить в начало списка - 1
- добавить в конец списка - O(n)
- получить N-й элемент - O(n)
- конкатернация двух списков - O(n)

Поэтому всегда работают с началом списка. А конкатернации можно и нужно избегать, используя вместо нее io_list.

Могут быть какой угодно вложенности
[1,2,[1,2,3],3]

Могут быть правильные (proper) и неправильные (nonproper)
[1,2,3]
[1 | [2,3]]
[1,2 | [3]]
[1,2 | 3]


Преимущетсво array перед связаным списком -- это константное время доступа к любому элементу. 
В большинстве функциональных языков такой структуры данных нет. Но в Erlang есть, хотя им редко пользуются. Реализация не документирована, и имеет ли array в Erlang свои преимущетсва я не знаю.
 The representation is not documented and is subject to change without notice.
Можно предположить, что либо модуль реализован полностью на BIF, либо как-то поверх list. Во втором случае он не будет иметь никаких преимуществ.

proplists
CRUD:
add item: [{Key, Value} | List]
get item: proplists:get_value(Key, List) % (Key, List, Default)
delete: proplists:delete(Key, List)
update функции готовой нет. Можно просто добавить элемент с тем же ключом в начало, и get\_value будет возвращать его. А можно
[{Key, Value} | proplists:delete(Key, List)]
Из этого следует, что в списке могут быть несколько значений с одним ключом
get\_all\_values
Используется как небольшое KV хранилище. Не больше нескольких десятков элементов.
Часто для хранения и передачи настроек или опций.


io_list
Очень удобная структура данных. Особенно в работе с сокетами, или для вебсервера.
Обычно данные, которые нужно отправить клиенту, генерируются кусками из разных источников. И потом все эти куски сшить. То есть, прибегнуть к конкатернации. А она, как мы знаем, дорога.
Есть выход, данные можно не сшивать сразу, а собирать во вложенные массивы
Part1 = [1,2,3]
Part2 = [4,5,6]
Part3 = [Part1, Part2]
All = [8,9,Part3,10,11]
И в конце один раз сделать
lists:flatten(All)
Эта функция делает из массивов любой вложенности плоский массив за один проход. O(n).

Итак io\_list это list, включающий byte(), binary(), io_list()
польза от него в том, что сокеты принимают его как есть, без flatten и без преобразований в binary


dict


есть еще orddict и gb_trees (general balanced trees)
orddict хранит ключи в сортированом виде, поэтому быстрый поиск по ключу, но медленное добавление
gb\_trees хранит ключи, понятно, в дереве. Поэтому еще более быстрый поиск O(ln(n)), но добавление может оказаться медленным, из-за необходимости перебалансировать дерево.


ets


dets, mnesia


Итого, из KV структур данных у нас: proplists, dict, orddict, gb_trees и ets
прилично. На небольших данных, как вы понимаете, разницы нет.
И я использую самое простое -- proplist, он понятно выводится в консоль )
А для больших данных я сразу беру тяжелую артиллерию -- ets 
Вот и весь мой рецепт :)



### Обработка списков с помощью рекурсивных функций с аккумуляторами


### Функции высшего порядка: map, filter, foldl


### List Comprehensions

но я не очень часто использую, препочитаю map и filter )

### Binary, pattern matching with binary
