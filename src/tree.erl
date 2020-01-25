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
-export([empty/0, insert/3, lookup/2, remove/2]).

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

remove(_, {node, nil}) -> empty();
remove(Key, {node, {Key, _, {node, nil}, {node, nil}}}) -> empty();
remove(Key, {node, {Key, _, {node, nil}, Biggest}}) -> Biggest;
remove(Key, {node, {Key, _, Smaller, {node, nil}}}) -> Smaller;
remove(Key, {node, {Key, _, Smaller, Bigger}}) ->
  {NewKey, NewVal} = get_smallest(Bigger),
  NewBigger = remove(NewKey, Bigger),
  {node, {NewKey, NewVal, Smaller, NewBigger}};
remove(Key, {node, {NodeKey, Val, Smaller, Bigger}}) when NodeKey > Key ->
  {node, {NodeKey, Val, remove(Key, Smaller), Bigger}};
remove(Key, {node, {NodeKey, Val, Smaller, Bigger}}) when NodeKey < Key ->
  {node, {NodeKey, Val, Smaller, remove(Key, Bigger)}}.
