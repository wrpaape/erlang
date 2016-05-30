-module(scratch).
-export([tuple_to_list/1, quick_sort/2, my_spawn/3, loop/2]).


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

quick_sort(_Compare, []) -> [].


my_spawn(Mod, Func, Args) ->
  TimeStart = erlang:timestamp(),
  {Pid, Ref} = spawn_monitor(Mod, Func, Args),
  timer:exit_after(5000, Pid, oogity_boogity),
  receive
    {'DOWN', Ref, process, Pid, Why} ->
      io:format("Ref:  ~p~nPid:  ~p~nWhy:  ~p~nDiff: ~p~n",
               [Ref, Pid, Why, timer:now_diff(erlang:timestamp(), TimeStart)])
  end.

loop(Sleep, Msg) ->
  timer:sleep(Sleep),
  io:format("~p~n", [Msg]),
  loop(Sleep, Msg).

