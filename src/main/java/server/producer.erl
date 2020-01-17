-module(producer).

-import(login, [logout/1]).

-export([producer/2]).

-include("protos.hrl").

producer(Sock, Username) ->
    receive
        {tcp, _, Data} ->
            ProducerInfo = protos:decode_msg(Data, 'Produce'),
            Product = maps:find(productName, ProducerInfo),
            negotiatorsHandler ! {new_producer, Product, Username, self(), Data};
        {tcp_closed, _} ->
            logout(Username);
        {tcp_error, _} ->
            logout(Username);
        {timeout, negotiatorsHandler, Data} ->
            gen_tcp:send(Sock, Data)
    end.