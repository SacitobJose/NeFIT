-module(importer).
-export([importer/1]).

importer(Sock, Username) ->
    receive 
        {tcp, _, Data} ->
            ImporterInfo = protos:decode_msg(Data, 'Import'),
            Product = maps:find(productName, ImporterInfo),
            negotiatorsHandler ! {new_importer, Product, Username, self(), Data};
        {tcp_closed, _} ->
            logout(Username);
        {tcp_error, _} ->
            logout(Username);
        {producer, negotiatorsHandler, ProducerName} ->
            ToSend = protos:encode_msg(#'ResponseImport'{producerName = ProducerName}),
            gen_tcp:send(Sock, ToSend)
    end.