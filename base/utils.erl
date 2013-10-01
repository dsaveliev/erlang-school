-module(utils).
-author('Yura Zhloba <yzh44yzh@gmail.com>').

-export([next_day/1, prev_day/1, add_days/2, days_range/2, prev_month/1]).
-export([init_crypto/0, rand_bstr/1, shuffle/1]).


%%% calendar functions

-spec(next_day(calendar:date()) -> calendar:date()).
next_day(Date) -> add_days(Date, 1).


-spec(prev_day(calendar:date()) -> calendar:date()).
prev_day(Date) -> add_days(Date, -1).


-spec(add_days(calendar:date(), integer()) -> calendar:date()).
add_days({Y, M, D}, Days) ->
    GD = calendar:date_to_gregorian_days(Y, M, D),
    calendar:gregorian_days_to_date(GD + Days). 


days_range(FromDate, TillDate) when FromDate < TillDate ->
    days_range(FromDate, TillDate, []).

days_range(TillDate, TillDate, Acc) -> lists:reverse(Acc);

days_range(CurrDate, TillDate, Acc) ->    
    NewAcc = [CurrDate | Acc],
    days_range(next_day(CurrDate), TillDate, NewAcc).


prev_month({Year, 1}) -> {Year - 1, 12};
prev_month({Year, Month}) -> {Year, Month - 1}.


%% random values

init_crypto() ->
    <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
    random:seed({A,B,C}).


-spec(rand_bstr(integer()) -> binary()).
rand_bstr(Length) ->
    GetChar = fun(Char) -> if
                               Char > 83 -> Char + 13;
                               Char > 57 -> Char + 7;
                               true -> Char
                           end
              end,
    Str = [GetChar(crypto:rand_uniform(48, 110)) || _ <- lists:seq(1, Length)],
    list_to_binary(Str).


-spec(shuffle(list()) -> list()).
shuffle(List) ->
    List2 = [{random:uniform(), Item} || Item <- List],
    List3 = lists:sort(List2),
    [Item || {_Rand, Item} <- List3].
    

