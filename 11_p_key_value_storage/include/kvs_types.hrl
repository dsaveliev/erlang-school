-type(key() :: atom() | string() | binary()).
-type(value() :: term()).

-define(ERROR(Format, Data), error_logger:error_msg(Format, Data)).

-type(key() :: binary() | string() | atom()).
-type(value() :: term()).
-type(value_time() :: {calendar:datetime(), value()}).
-type(key_set() :: {key(), [value_time()]}).
