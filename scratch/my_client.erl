-module(my_client).
-export([start_server/0, server_ls/0, get_file/1, tell_joke/0]).

start_server() ->
  code:ensure_loaded(my_server),
  register(my_server_instance, my_server:start(".")),
  ok.


server_ls() ->
  ping_server(list_dir).


get_file(File) ->
  ping_server({get_file, File}).


tell_joke() ->
  lists:foreach(fun(Request) ->
                    io:format("~p~n",
                              [Request]),
                    timer:sleep(250),
                    io:format("~s~n",
                              [ping_server(Request)]),
                    timer:sleep(250)
                end,
                [knock_knock, "kill", kill_urself]).


ping_server(Query) ->
  whereis(my_server_instance) ! {self(), Query},
  receive
    {_, Response} -> Response;
    Response      -> Response
  end.



