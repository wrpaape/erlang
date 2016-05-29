-module(sort).
-export([quick_sort/1]).


quick_sort([])            -> [];
quick_sort([Pivot | Rem]) ->
  quick_sort([X || X <- Rem, X < Pivot])
  ++ [Pivot] ++
  quick_sort(lists:filter(fun(X) ->
                              X >= Pivot
                          end, Rem)).
