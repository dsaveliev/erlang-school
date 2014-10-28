-module(base).

-export([reverse/1, partition/2, zip/2, map/2, filter/2, foldl/3]).

%%%%

reverse(List) ->
    reverse(List, []).

reverse([], Acc) -> Acc;
reverse([Head | Tail], Acc) -> reverse(Tail, [Head | Acc]).

%%%%

partition(Pred, List) ->
    partition(Pred, List, [], []).

partition(_Pred, [], TrueList, FalseList) -> {reverse(TrueList), reverse(FalseList)};
partition(Pred, [Head | Tail], TrueList, FalseList) ->
    case Pred(Head) of
        true -> partition(Pred, Tail, [Head | TrueList], FalseList);
        false -> partition(Pred, Tail, TrueList, [Head | FalseList])
    end.

%%%%

zip(List1, List2) ->
    zip(List1, List2, []).

zip([], _, Acc) -> reverse(Acc);
zip(_, [], Acc) -> reverse(Acc);
zip([Head1 | Tail1], [Head2 | Tail2], Acc) ->
    zip(Tail1, Tail2, [{Head1, Head2} | Acc]).

%%%%

map(Fun, List) ->
    map(Fun, List, []).

map(_Fun, [], Acc) -> reverse(Acc);
map(Fun, [Head | Tail], Acc) -> map(Fun, Tail, [Fun(Head) | Acc]).

%%%%

filter(Pred, List) ->
    filter(Pred, List, []).

filter(_Pred, [], Acc) -> reverse(Acc);
filter(Pred, [Head | Tail], Acc) ->
    case Pred(Head) of
        true -> filter(Pred, Tail, [Head | Acc]);
        false -> filter(Pred, Tail, Acc)
    end.

%%%%

foldl(_Fun, Acc, []) -> Acc;
foldl(Fun, Acc, [Head | Tail]) ->
    Acc2 = Fun(Head, Acc),
    foldl(Fun, Acc2, Tail).
