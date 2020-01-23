-module(producer).

-import(login, [logout/1]).

-export([producer/2]).

-include("protos.hrl").

producer(Sock, Username) ->
    Handler = spawn(fun() -> producer(Sock, Username, 0) end),
    {ok, Length} = gen_tcp:recv(Sock, 4),
    {ok, Data} = gen_tcp:recv(Sock, binary:decode_unsigned(Length)),
    {_, {produce, DataProduce}} = protos:decode_msg(Data, 'Transaction'),
    {_, Product, _, _, _, _} = protos:decode_msg(DataProduce, 'Produce'),
    Handler ! {new_producer, Username, Product, <<Length, Data/binary>>},
    producer(Sock, Username, Handler, 0).

producer(Sock, Username, Handler, 0) ->
    {ok, Length} = gen_tcp:recv(Sock, 4),
    {ok, Data} = gen_tcp:recv(Sock, binary:decode_unsigned(Length)),
    {_, {produce, DataProduce}} = protos:decode_msg(Data, 'Transaction'),
    {_, Product, _, _, _, _} = protos:decode_msg(DataProduce, 'Produce'),
    Handler ! {new_producer, Username, Product, <<Length, Data/binary>>},
    producer(Sock, Username, Handler, 0).

producer(Sock, Username, 0) ->
    receive
        {new_producer, Username, Product, Data} ->
            negotiatorsHandler ! {new_producer, Username, self(), Product, Data},
            producer(Sock, Username, 0);
        {timeout, negotiatorsHandler, ToSend} ->
            gen_tcp:send(Sock, ToSend),
            producer(Sock, Username, 0)
    end.