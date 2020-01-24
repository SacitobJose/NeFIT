-module(negotiator).

-export([negotiator/1]).
-import(server, [padding/1]).

-include("protos.hrl").

negotiator(Sock) ->
    PID1 = self(),
	PID2 = spawn(fun() -> negotiatorSocket(Sock, PID1) end),
    gen_tcp:controlling_process(Sock, PID2),
    negotiator(Sock, #{}, #{}).

negotiator(Sock, Importers, Producers) ->
    receive
        {new_producer, Username, PID, ProductName, Data} ->
            gen_tcp:send(Sock, Data),
            case maps:find(ProductName, Producers) of
                {ok, ProducersProduct} ->
                    NewProducersProduct = maps:put(Username, PID, ProducersProduct),
                    NewProducers = maps:put(ProductName, NewProducersProduct, Producers),
                    negotiator(Sock, Importers, NewProducers);
                _ ->
                    Map = maps:new(),
                    NewProducersProduct = maps:put(Username, PID, Map),
                    NewProducers = maps:put(ProductName, NewProducersProduct, Producers),
                    negotiator(Sock, Importers, NewProducers)
            end;
        {new_importer, Username, PID, ProductName, Data} ->
            gen_tcp:send(Sock, Data),
            case maps:find(ProductName, Importers) of
                {ok, ImportersProduct} ->
                    NewImportersProduct = maps:put(Username, PID, ImportersProduct),
                    NewImporters = maps:put(ProductName, NewImportersProduct, Importers),
                    negotiator(Sock, NewImporters, Producers);
                _ ->
                    Map = maps:new(),
                    NewImportersProduct = maps:put(Username, PID, Map),
                    NewImporters = maps:put(ProductName, NewImportersProduct, Producers),
                    negotiator(Sock, NewImporters, Producers)
            end;
        {timeoutProducer, ProducerName, ProductName, Data} ->
            {ok, ProducersProduct} = maps:find(ProductName, Producers),
            {ok, Producer} = maps:find(ProducerName, ProducersProduct),
            Oneof = protos:encode_msg(#'Response'{res = {producer, Data}}),
		    Producer ! {timeout, negotiatorsHandler, Oneof},
            NewProducers = maps:remove(ProducerName, ProducersProduct),
            negotiator(Sock, Importers, maps:put(ProductName, NewProducers, Producers));
        {timeoutImporter, ProducerName, ProductName, SaleInfo} ->
            {_, Username, Quantity, Price} = protos:decode_msg(SaleInfo, 'SaleInfo'),
            {ok, ImportersProduct} = maps:find(ProductName, Importers),
            {ok, Importer} = maps:find(Username, ImportersProduct),
            ImporterResponse = #'ImporterResponse'{producerName = ProducerName, productName = ProductName, quantity = Quantity, price = Price},
            Oneof = protos:encode_msg(#'Response'{res = {importer, ImporterResponse}}),
            X = padding(binary:encode_unsigned(byte_size(Oneof))),
            Data = <<X/binary, Oneof/binary>>,
            Importer ! {timeout, negotiatorsHandler, Data},
            NewImporters = maps:remove(Username, ImportersProduct),
            negotiator(Sock, maps:put(ProductName, NewImporters, Importers), Producers)
	end.

negotiatorSocket(Sock, Handler) ->
    {ok, Length} = gen_tcp:recv(Sock, 4),
    {ok, Data} = gen_tcp:recv(Sock, binary:decode_unsigned(Length)),
    {_, Success, Producer, Product, SalesInfo} = protos:decode_msg(Data, 'DealerTimeout'),
    Proto = #'DealerTimeout'{Success = Success, producerName = ProducerName, productName = ProductName, sales = SalesInfo},
    if
        Success == true ->
            Handler ! {timeoutProducer, Producer, Product, Proto},
            getImporters(Handler, SalesInfo, Producer, Product),
            negotiatorSocket(Sock, Handler);
        Success == false ->
            Handler ! {timeoutProducer, Producer, Product, Proto},
            negotiatorSocket(Sock, Handler)
    end.

getImporters(_, [], _, _) -> 
	true;
getImporters(Handler, SalesInfoList, Producer, Product) ->
	SaleInfo = lists:nth(1, SalesInfoList),
    Handler ! {timeoutImporter, Producer, Product, SaleInfo},
    getImporters(Handler, lists:nthtail(1, SalesInfoList), Producer, Product).
