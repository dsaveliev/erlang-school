-type(user() :: binary()).
-type(text() :: binary()).
-type(message() :: {user(), text()}).

-type(connection() :: pid()).
-type(client() :: {user(), pid()}).
