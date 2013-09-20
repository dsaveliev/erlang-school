-type(key() :: atom() | string() | binary()).
-type(value() :: term()).

-define(ERROR(Format, Data), error_logger:error_msg(Format, Data)).
