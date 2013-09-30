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


### Структуры данных: list, array, proplist, io_list, dict, ets

list -- рабочая лошадка всех функциональных языков. Основная структура данных. А в некоторых языках (в Lisp) -- вообще единственная структура данных. Представляет собой однонаправленный связанный список.


### Обработка списков с помощью рекурсивных функций с аккумуляторами


### Функции высшего порядка: map, filter, foldl


### List Comprehensions


### Binary, pattern matching with binary
