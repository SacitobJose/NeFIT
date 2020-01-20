-module(negotiator).

-export([negotiator/3]).

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
      {tcp, _, Data} ->
		% decode data
		Map_Data = decode(Data),
		% get product name
		{ok, ProductName} = maps:find(productName, Map_Data),
		% send info to producer
		{ok, ProducerName} = maps:find(producerName, Map_Data),
		{ok, ProducersProduct} = maps:find(ProductName, Producers),
		{ok, Producer} = maps:find(ProducerName, ProducersProduct),
		Producer ! {timeout, negotiatorsHandler, io_lib:format("Producer_~s~n", [binary_to_list(string:trim(Data))])},
		NewProducers = maps:remove(ProducerName, ProducersProduct),
		% extract importers' usernames, send them the info needed
		{ok, ImportersBuying} = maps:find(importers, Map_Data),
		case maps:find(ProductName, Importers) of
			{ok, ImportersProduct} ->
				NewImporters = sendImporters(ImportersProduct, ImportersBuying, binary_to_list(ProducerName), binary_to_list(ProductName)),
				negotiator(Sock, maps:put(ProductName, NewImporters, Importers), maps:put(ProductName, NewProducers, Producers));
			_ ->
				negotiator(Sock, Importers, maps:put(ProductName, NewProducers, Producers))
		end
    end.

% Decode what came from the negotiator
decode(Data) ->
    Message = string:split(string:trim(Data), "_", all),
    ProducerName = lists:nth(3, Message),
    ProductName = lists:nth(4, Message),
    Map = maps:new(),
    Map2 = maps:put(producerName, ProducerName, Map),
    Map3 = maps:put(productName, ProductName, Map2),
    Importers = lists:nthtail(4, Message),
    maps:put(importers, Importers, Map3).

% Sends producer's username and product to each importer and extract them from importers' map
sendImporters(Importers, [], _, _) -> 
	Importers;
sendImporters(Importers, Buying, Producer, Product) ->
	Username = lists:nth(1,Buying),
	Quantity = binary_to_list(lists:nth(2,Buying)),
	Price = binary_to_list(lists:nth(3,Buying)),
    {ok, PID} = maps:find(Username, Importers),
    PID ! {producer, negotiatorsHandler, io_lib:format("Importer_~s_~s_~s_~s~n", [Producer, Product, Quantity, Price])},
    sendImporters(maps:remove(Username, Importers), lists:nthtail(3, Buying), Producer, Product).
