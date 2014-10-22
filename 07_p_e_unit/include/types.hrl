-type(some() :: term()).

-type(day_of_week() :: 1..7).
-type(user_id() :: integer()).
-type(user_name() :: string()).
-type(user() :: {user_id(), user_name()}).

-record(user, {
          id :: user_id(),
          name :: user_name()
         }).
-type(user_list() :: [#user{}]).

-type(date() :: calendar:date()).
-type(num_days() :: integer()).
