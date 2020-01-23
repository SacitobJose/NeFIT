-module(importer).

-import(login, [logout/1]).

-export([importer/2]).

-include("protos.hrl").

importer(Sock, Username) ->
    PID1 = self(),
    PID2 = spawn(fun() -> importerSocket(Sock, Username, PID1) end),
    gen_tcp:controlling_process(Sock, PID2),
    importer(Sock, Username, 0).

importerSocket(Sock, Username, Handler) ->
    {ok, Length} = gen_tcp:recv(Sock, 4),
    {ok, Data} = gen_tcp:recv(Sock, binary:decode_unsigned(Length)),
    {_, {import, DataImport}} = protos:decode_msg(Data, 'Transaction'),
    {_, Product, _, _, _, _} = DataImport,
    Handler ! {new_importer, Username, Product, <<Length/binary, Data/binary>>},
    importerSocket(Sock, Username, Handler).

importer(Sock, Username, 0) ->
    receive
        {new_importer, Username, Product, Data} ->
            negotiatorsHandler ! {new_importer, Username, self(), Product, Data},
            importer(Sock, Username, 0);
        {timeout, negotiatorsHandler, ToSend} ->
            gen_tcp:send(Sock, ToSend),
            importer(Sock, Username, 0)
    end.
