-module(negotiator).

-export([negotiator/3]).

negotiator(Sock, Importers, Producers) ->
    io:format("Negotiator actor initiated~n", []),
    receive
      {new_producer, Username, PID, ProductName, Data} ->
		gen_tcp:send(Sock, Data),
		NewProducers = maps:put(ProductName, {Username, PID}, Producers),
		negotiator(Sock, Importers, NewProducers);
      {new_importer, Username, PID, ProductName, Data} ->
		gen_tcp:send(Sock, Data),
		NewImporters = maps:put(ProductName, {Username, PID}, Importers),
		negotiator(Sock, NewImporters, Producers);
      {tcp, _, Data} ->
		% decode data
		Map_Data = decode(Data),
		% send info to producer
		{ok, ProducerName} = maps:find(producerName, Map_Data),
		{ok, Producer} = maps:find(ProducerName, Producers),
		Producer ! {timeout, negotiatorsHandler, Data},
		% get product name
		{ok, ProductName} = maps:find(productName, Map_Data),
		% extract importers' usernames, send them the producer's info and remove them from their map
		{ok, ImportersBuying} = maps:find(importers, Map_Data),
		NewImporters = sendImporters(ImportersProduct, ImportersBuying),
		negotiator(Sock, NewImporters,
				maps:remove(ProducerName, Producers))
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
sendImporters(Importers, []) -> Importers;
sendImporters(Importers, [H|T]) ->
    {ok, PID} = maps:find(H, Importers),
    PID ! {producer, negotiatorsHandler, io:format("Success~n", [])},
    sendImporters(maps:remove(H, Importers), T).
