-module(producer).

-import(login, [logout/1]).

-export([producer/2]).

producer(Sock, Username) ->
    receive
      {tcp, _, Data} ->
        Message = string:split(string:trim(Data), "_", all),
        Product = lists:nth(3, Message),
        negotiatorsHandler ! {new_producer, Username, self(), Product, Data},
        producer(Sock, Username);
      {tcp_closed, _} ->
        logout(Username);
      {tcp_error, _} ->
        logout(Username);
      {timeout, negotiatorsHandler, ToSend} ->
	      gen_tcp:send(Sock, ToSend),
        producer(Sock, Username)
    end.
