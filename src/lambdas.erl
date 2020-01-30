%%%-------------------------------------------------------------------
%%% @author akorchyn
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. Jan 2020 4:39 PM
%%%-------------------------------------------------------------------
-module(lambdas).
-author("akorchyn").
-compile(export_all).

one() -> 1.
two() -> 2.
add(X, Y) -> X() + Y().

map(_, []) -> [];
map(F, [H|Tail]) -> [F(H) | map(F, Tail)].

increment(List) -> map(fun(X) -> X + 1 end, List).
decrement(List) -> map(fun(X) -> X - 1 end, List).

filter(Pred, L) -> lists:reverse(filter(Pred, L, [])).
filter(_, [], Result) -> Result;
filter(Pred, [H|T], Result) ->
  case Pred(H) of
    true -> filter(Pred, T, [H|Result]);
    false -> filter(Pred, T, Result)
  end.

fold(_, Res, []) -> Res;
fold(Op, Res, [H|T]) -> fold(Op, Op(H, Res), T).

reverse(List) -> fold(fun(H, T) -> [H | T] end, [], List).
map2(F, List) -> reverse(fold(fun(A, B) -> [F(A) | B] end, [], List)).