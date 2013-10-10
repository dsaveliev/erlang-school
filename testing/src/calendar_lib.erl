-module(calendar_lib).

% public api
-export([prev_date/1, next_date/1, date_range/2]).

-ifdef(TEST).
-export([add_days/2]).
-endif.

-include("types.hrl").

-spec(prev_date(date()) -> date()).
prev_date(Date) ->
    add_days(Date, -1).

-spec(next_date(date()) -> date()).
next_date(Date) ->
    add_days(Date, 1).

-spec(add_days(date(), num_days()) -> date()).
add_days(Date, NumDays) ->
    GDays = calendar:date_to_gregorian_days(Date),
    calendar:gregorian_days_to_date(GDays + NumDays).

-spec(date_range(date(), date()) -> [date()] | error).
date_range(DateFrom, DateTo) when DateTo < DateFrom ->
    error;
date_range(DateFrom, DateTo) ->
    date_range(DateFrom, DateTo, []).

-spec(date_range(date(), date(), [date()]) -> [date()]).
date_range(DateFrom, DateFrom, Acc) -> [DateFrom | Acc];
date_range(DateFrom, DateTo, Acc) ->
    NewAcc = [DateTo | Acc],
    PrevDate = prev_date(DateTo),
    date_range(DateFrom, PrevDate, NewAcc).
