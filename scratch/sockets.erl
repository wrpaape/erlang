-module(sockets).
-export([get_url/0,
         get_url/1]).


-spec get_url() -> binary().
get_url() -> get_url("www.google.com").


-spec get_url(Host) -> binary() when
    Host :: atom()
          | string()
          | {byte(), byte(), byte(), byte()}
          | {char(), char(), char(), char(), char(), char(), char(), char()}.

get_url(Host) ->
  {ok, Socket} = gen_tcp:connect(Host, 80, [binary, {packet, 0}]),
  ok = gen_tcp:send(Socket, "GET / HTTP/1.0\r\n\r\n"),
  receive_data(Socket, []).



receive_data(Socket, BinAcc) ->
  receive
    {tcp, Socket, Bin} ->
      receive_data(Socket, [Bin | BinAcc]);

    {tcp_closed, Socket} ->
      list_to_binary(lists:reverse(BinAcc))
  end.
