-module(my_server).
-export([start/1, loop/1]).

start(Dir) ->
  spawn(my_server, loop, [Dir]).

loop(Dir) ->
  receive
    {Client, list_dir} ->
      Client ! {self(), file:list_dir(Dir)};

    {Client, {get_file, File}} ->
      Full = filename:join(Dir, File),
      Client ! {self(), file:read_file(Full)};

    {Client, knock_knock} ->
      Client ! {self(), "Who's there?"};

    {Client, kill_urself} ->
      Client ! "N-NANI????",
      exit("Katana'd");

    {Client, Somebody} ->
      Client ! {self(), Somebody ++ " who?"}
  end,
  loop(Dir).
