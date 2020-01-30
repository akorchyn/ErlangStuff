%%%-------------------------------------------------------------------
%%% @author akorchyn
%%% @copyright (C) 2020
%%% @doc
%%%
%%% @end
%%% Created : 25. Jan 2020 8:25 PM
%%%-------------------------------------------------------------------

-module(tree).
-author("akorchyn").
-export([empty/0, insert/3, lookup/2, remove/2, get_smallest/1, has_value/2, has_value_exception/2]).

empty()->{node, nil}.


insert(Key, Val, {node, nil}) -> {node, {Key, Val, empty(), empty()}};
insert(NewKey, NewVal, {node, {Key, Val, Smaller, Bigger}}) when NewKey > Key ->
  {node, {Key, Val, Smaller, insert(NewKey, NewVal, Bigger)}};
insert(NewKey, NewVal, {node, {Key, Val, Smaller, Bigger}}) when NewKey < Key ->
  {node, {Key, Val, insert(NewKey, NewVal, Smaller), Bigger}};
insert(Key, Val, {node, {Key, _, Smaller, Bigger}}) ->
  {node, {Key, Val, Smaller, Bigger}}.


lookup(_, {node, nil}) -> undefined;
lookup(Key, {node, {Key, Val, _, _}}) -> {ok, Val};
lookup(ToFind, {node, {Key, _, _, Bigger}}) when ToFind > Key -> lookup(ToFind, Bigger);
lookup(ToFind, {node, {Key, _, Smaller, _}}) when ToFind < Key -> lookup(ToFind, Smaller).


get_smallest({node, nil}) -> undefined;
get_smallest({node, {Key, Val, {node, nil}, _}}) -> {Key, Val};
get_smallest({node, {_, _, Smaller, nil}}) -> get_smallest(Smaller).


remove(Key, Tree) ->
  case catch remove_impl(Key, Tree) of
    Var -> Var
  end.
remove_impl(_, {node, nil}) -> throw(undefined), empty();
remove_impl(Key, {node, {Key, _, {node, nil}, {node, nil}}}) -> empty();
remove_impl(Key, {node, {Key, _, {node, nil}, Biggest}}) -> Biggest;
remove_impl(Key, {node, {Key, _, Smaller, {node, nil}}}) -> Smaller;
remove_impl(Key, {node, {Key, _, Smaller, Bigger}}) ->
  {NewKey, NewVal} = get_smallest(Bigger),
  NewBigger = remove_impl(NewKey, Bigger),
  {node, {NewKey, NewVal, Smaller, NewBigger}};
remove_impl(Key, {node, {NodeKey, Val, Smaller, Bigger}}) when NodeKey > Key ->
  {node, {NodeKey, Val, remove_impl(Key, Smaller), Bigger}};
remove_impl(Key, {node, {NodeKey, Val, Smaller, Bigger}}) when NodeKey < Key ->
  {node, {NodeKey, Val, Smaller, remove_impl(Key, Bigger)}}.


has_value(_, {node, nil}) -> false;
has_value(Value, {node, {_, Value, _, _}}) -> true;
has_value(Value, {node, {_, _, Left, Right}}) ->
  case has_value(Value, Left) of
    true -> true;
    false -> has_value(Value, Right)
  end.


has_value_exception(Value, Tree) ->
  try has_value_exception_impl(Value, Tree) of
    false -> false
  catch
    true -> true
  end.

has_value_exception_impl(_, {node, nil}) -> false;
has_value_exception_impl(Value, {node, {_, Value, _, _}}) -> throw(true);
has_value_exception_impl(Value, {node, {_, _, Left, Right}}) ->
    has_value_exception_impl(Value, Left), has_value_exception_impl(Value, Right).
