%%%-------------------------------------------------------------------
%%% @author akorchyn
%%% @doc
%%%
%%% @end
%%% Created : 25. Jan 2020 4:16 PM
%%%-------------------------------------------------------------------
-module(npuzzle).
-compile(export_all).

head([H|_]) -> H.
tail([_|T]) -> T.
second([_,S|_]) -> S.

get_nth_element(1, [H|_]) -> H;
get_nth_element(N, [_|T]) -> get_nth_element(N - 1, T);
get_nth_element(_, _) -> io:format("Sorry, I can't parse it~n").

same(X, X) -> true;
same(_,_) -> false.

oh_god(N) ->
  if N =:= 2 -> might_succeed;
    true -> always_does  %% this is Erlang's if's 'else!'
  end.

beach(Temperature) ->
  case Temperature of
    {celsius, N} when N >= 20, N =< 45 ->
      'favorable';
    {kelvin, N} when N >= 293, N =< 318 ->
      'scientifically favorable';
    {fahrenheit, N} when N >= 68, N =< 113 ->
      'favorable in the US';
    _ ->
      'avoid beach'
  end.

factorial(N) -> factorial(N, 1).
factorial(0, Res) -> Res;
factorial(N, Res)  -> factorial( N - 1, Res * N).

count(List) -> count(List, 0).
count([], N) -> N;
count([_|T], N) -> count(T, N+1).

duplicate(Var, N) -> duplicate(Var, N, []).
duplicate(_, 0, List) -> List;
duplicate(Var, N, List) when N > 0 -> duplicate(Var, N - 1, [Var | List]).

sublist(List, N) -> sublist(List, N, []).
sublist(_, 0, New) -> New;
sublist([], _, New) -> New;
sublist([H|T], N, New) when N > 0 -> sublist(T, N - 1, New ++ [H]).

zip(ListA, ListB) -> lists:reverse(zip(ListA, ListB, [])).
zip([], [], Result) -> Result;
zip([], _, Result) -> Result;
zip(_, [], Result) -> Result;
zip([H1|T1], [H2|T2], Result) -> zip(T1, T2, [{H1, H2}|Result]).

partition(Number, List) -> partition(Number, List, [], [], []).
partition(_, [], Smaller, Bigger, Equal) -> {Smaller, Equal, Bigger};
partition(Number, [H|T], Smaller, Bigger, Equal) when Number > H -> partition(Number, T, Smaller ++ [H], Bigger, Equal);
partition(Number, [H|T], Smaller, Bigger, Equal) when Number < H -> partition(Number, T, Smaller, Bigger ++ [H], Equal);
partition(Number, [H|T], Smaller, Bigger, Equal) when Number =:= H -> partition(Number, T, Smaller, Bigger, Equal ++ [H]).

quick_sort([]) -> [];
quick_sort([H|List]) ->
  {Smaller, Equal, Bigger} = partition(H, List, [], [], []),
  quick_sort(Smaller) ++ [H|Equal] ++ quick_sort(Bigger).