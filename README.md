# Короткий курс Erlang 
3-х дневная программа обучения Erlang.

Расчитана на аудиторию, имеющую некоторые базовые знания языка, но не
применявшую Erlang на практике. Цель программы -- пересмотреть базовые
знания, познакомить с инфраструктурой, инструментами, библиотеками,
показать, как на практике идет работа с Erlang проектами.


## День 1й

### Основы Erlang (2 часа)

Работа в erlang-shell. 

atom, tuple, record

pattern matching

case, if, guards

function, close, module, анонимные функции

Структуры данных: list, proplist, io_list, dict, ets

Обработка списков с помощью рекурсивных функций с аккумуляторами

Функции высшего порядка: map, filter, foldl

List Comprehensions

Binary, pattern matching with binary


### Concurrency (1 час)

Concurrency and parallelism

Многопоточность в Erlang

Жизнь erlang-процесса
(бесконечная рекурсия, состояние на стеке)

Работа с потоками на низком уровне
spawn, receive, register
link, spawn_link, monitor, exit, trap exit


### OTP (2 часа)

gen_server

supervisor

Обработка ошибок
Defensive Programming vs Let it crash

application

Типичная структура OTP проекта
с дочерними приложениями и зависимыми библиотеками


### Работа с сокетами, практика (1 час)

TCP, UDP

Активный и пассивный режим сокета

Наивный tcp сервер

http сервер и клиент


## День 2й

### Инструменты (2 часа)

rebar
управление зависимостями
сборка проекта

Логирование
report browser
elogger

appmon, pman, tv, webtool

dialyzer


### Тестирование, практика (2 часа)

eunit

Тестирование gen_server

Тестирование базы данных

Тестирование клиентом


### Библиотеки (2 часа)

ranch acceptor pool

Веб сервера: yaws, cowboy

Сериализация данных: JSON (jsonx), bert, google protocol buffers

poolboy

epgsql


## День 3й, практика (6 часов)

Пишем свое key-value хранилище,
чтобы было интересно, с фичами:
- несколько value для одного key, различаемых по timestamp (как в HBase)
- сохранение на диск и восстановление
- TCP интерфейс (доступ через telnet)
- HTTP интерфейсом (используем cowboy)
- журналирование

Развертывание и запуск сервера

Подключение к серверу через удаленную консоль, мониторинг

Стресс-тестирование
профилирование, выявление узких мест
