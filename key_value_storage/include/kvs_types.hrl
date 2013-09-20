-type(name() :: string()).
-type(info() :: string()).

-define(ERROR(Format, Data), error_logger:error_msg(Format, Data)).
