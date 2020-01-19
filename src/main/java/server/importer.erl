-module(importer).

-import(login, [logout/1]).

-export([importer/2]).

importer(Sock, Username) ->
    receive 
        {tcp, _, Data} ->
            Message = string:split(string:trim(Data), "_", all),
            Product = lists:nth(2, Message),
            Protobuf = lists:nth(3, Message),
            negotiatorsHandler ! {new_importer, Product, Username, self(), Protobuf};
        {tcp_closed, _} ->
            logout(Username);
        {tcp_error, _} ->
            logout(Username);
        {producer, negotiatorsHandler, ToSend} ->
            gen_tcp:send(Sock, ToSend)
    end.