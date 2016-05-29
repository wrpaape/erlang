-module(scratch).
-export([tuple_to_list/1, quick_sort/2]).


-spec scratch:tuple_to_list(Tuple) -> List when
    Tuple :: tuple(),
    List  :: [any()].

tuple_to_list(Tuple) ->
  lists:map(fun(N) ->
                element(N, Tuple)
            end,
            lists:seq(1, tuple_size(Tuple))).



-spec scratch:quick_sort(Compare, List) -> List when
    Compare :: fun((any(), any()) -> boolean()),
    List    :: [any()].

quick_sort(Compare,  [Pivot | Rem]) ->
  {Small, Large} = lists:partition(fun(El) ->
                                       Compare(El, Pivot)
                                   end, Rem),
  quick_sort(Compare, Small) ++ [Pivot | quick_sort(Compare, Large)];
quick_sort(_Compare, [])            -> [].
