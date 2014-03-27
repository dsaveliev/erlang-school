-define(LOGGER, true).

-ifdef(LOGGER).
-define(INFO(Format, Data), lager:info(Format, Data)).
-define(INFO(Format), lager:info(Format)).
-else.
-define(INFO(Format, Data), do_nothing).
-define(INFO(Format), do_nothing).
-endif.

-define(WARN(Format, Data), lager:warning(Format, Data)).
-define(WARN(Format), lager:warning(Format)).

-define(ERROR(Format, Data), lager:error(Format, Data)).
-define(ERROR(Format), lager:error(Format)).

