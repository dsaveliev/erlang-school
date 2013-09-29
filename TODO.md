KVS
- HTTP интерфейсом (используем cowboy)
  - cowboy

=================

- мне нужно научиться работать с Erlang под виндой

- изучить разные способы запуска сервера
  (в рассылке было)
  
- повторить темы:
  - Структуры данных: list, proplist, io_list, dict, ets
    http://learnyousomeerlang.com/a-short-visit-to-common-data-structures
  - List Comprehensions
  - Binary, pattern matching with binary
  - supervisor
  - eunit

- попробовать тулы:
  - elogger
  - appmon, pman, tv, webtool
  - cprof, fprof
  - dialyzer
    - http://www.erlang.org/doc/apps/dialyzer/dialyzer_chapter.html
    - http://www.erlang.org/doc/man/dialyzer.html
    
- весь код написать заранее
  - основы языка
    - Структуры данных: list, proplist, io_list, dict, ets
    - Обработка списков с помощью рекурсивных функций с аккумуляторами
    - Функции высшего порядка: map, filter, foldl
    - List Comprehensions
    - Binary, pattern matching with binary
  + loop процесс, с обработкой сообщений
  + наивный tcp сервер
  - http сервер и клиент (with cowboy)
  - testing
    - eunit
    - Тестирование gen_server
    - Тестирование базы данных
    - Тестирование клиентом
  - key-value хранилище

инета не будет, поэтому нужно взять с собой инсталяторы Erlang под linux/win/mac
и все зависимости. И убедиться, что весь код собирается под linux/win/mac без инета
