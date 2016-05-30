-module(scratch).
-export([tuple_to_list/1, quick_sort/2, my_spawn/3, print_interval/2,
         bucket_start/0, bucket_listen/0, bucket_put/2, bucket_get/1]).
-define(RPC(Request), ?RPC(?MODULE, Request)).
-define(RPC(Process, Request),
        Process ! {self(), Request},
        receive
          {Process, Reply} -> Reply
        end).



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



-spec scratch:my_spawn(Mod, Func, Args) -> ok when
    Mod  :: atom(),
    Func :: atom(),
    Args :: [any()].

my_spawn(Mod, Func, Args) ->
  TimeStart = erlang:timestamp(),
  {Pid, Ref} = spawn_monitor(Mod, Func, Args),
  timer:exit_after(5000, Pid, oogity_boogity),
  receive
    {'DOWN', Ref, process, Pid, Why} ->
      io:format("Ref:  ~p~nPid:  ~p~nWhy:  ~p~nDiff: ~p~n",
               [Ref, Pid, Why, timer:now_diff(erlang:timestamp(), TimeStart)])
  end.



-spec scratch:print_interval(Sleep, Msg) -> none() when
    Sleep :: infinity | non_neg_integer(),
    Msg   :: any().

print_interval(Sleep, Msg) ->
  timer:sleep(Sleep),
  io:format("~p~n", [Msg]),
  print_interval(Sleep, Msg).



-spec scratch:bucket_start() -> true.

bucket_start() ->
  register(?MODULE,
           spawn(?MODULE, bucket_listen, [])).

-spec scratch:bucket_put(Key, Value) -> Reply when
    Key   :: any(),
    Value :: any(),
    Reply :: any().

bucket_put(Key, Value) -> ?RPC({put, Key, Value}).


-spec scratch:bucket_get(Key) -> Reply when
    Key   :: any(),
    Reply :: any().

bucket_get(Key) -> ?RPC({get, Key}).


bucket_listen() ->
  receive
    {From, {put, Key, Value}} ->
      put(Key, Value),
      From ! {?MODULE, true},
      bucket_listen();

    {From, {get, Key}}        ->
      From ! {?MODULE, get(Key)},
      bucket_listen()
  end.
