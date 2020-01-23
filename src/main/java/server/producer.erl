-module(producer).

-import(login, [logout/1]).

-export([producer/2]).

-include("protos.hrl").

producer(Sock, Username) ->
    PID1 = self(),
    PID2 = spawn(fun() -> producerSocket(Sock, Username, PID1) end),
    gen_tcp:controlling_process(Sock, PID2),
    producer(Sock, Username, 0).

producerSocket(Sock, Username, Handler) ->
    {ok, Length} = gen_tcp:recv(Sock, 4),
    {ok, Data} = gen_tcp:recv(Sock, binary:decode_unsigned(Length)),
    {_, {produce, DataProduce}} = protos:decode_msg(Data, 'Transaction'),
    {_, Product, _, _, _, _, _} = DataProduce,
    Handler ! {new_producer, Username, Product, <<Length/binary, Data/binary>>},
    producerSocket(Sock, Username, Handler).

producer(Sock, Username, 0) ->
    receive
        {new_producer, Username, Product, Data} ->
            negotiatorsHandler ! {new_producer, Username, self(), Product, Data},
            producer(Sock, Username, 0);
        {timeout, negotiatorsHandler, ToSend} ->
            gen_tcp:send(Sock, ToSend),
            producer(Sock, Username, 0)
    end.
