-module(producer).
-export([producer/1]).

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
    
    