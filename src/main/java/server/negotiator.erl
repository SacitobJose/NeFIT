-module(negotiator).

-export([negotiator/3]).

-include("protos.hrl").

negotiator(Sock, Importers, Producers) ->
    receive
        {new_producer, Username, PID, Data} ->
            gen_tcp:send(Sock, Data),
            NewProducers = maps:put(Username, PID, Producers),
            negotiator(Sock, Importers, NewProducers);
        {new_importer, Username, PID, Data} ->
            gen_tcp:send(Sock, Data),
            NewImporters = maps:put(Username, PID, Importers),
            negotiator(Sock, NewImporters, Producers);
        {tcp, _, Data} ->
            % decode data
            Map_Data = decode(Data),
            % send info to producer
            {ok, ProducerName} = maps:find(producerName, Map_Data),
            {ok, Producer} = maps:find(ProducerName, Producers),
            Producer ! {timeout, negotiatorsHandler, io_lib:format("Timeout:~p~n", [Data])},
            % get product name 
            {ok, ProductName} = maps:find(productName, Map_Data),
            % extract importers' usernames, send them the producer's info and remove them from their map
            {ok, ImportersBuying} = maps:find(sales, Map_Data),
            NewImporters = sendImporters(Importers, maps:to_list(ImportersBuying), ProducerName, ProductName),
            negotiator(Sock, NewImporters, maps:remove(ProducerName, Producers))
    end.

% Decode what came from the negotiator
decode(Data) ->
    Message = string:split(string:trim(Data), ":", all),
    ProducerName = lists:nth(3, Message),
    ProductName = lists:nth(4, Message),
    Map = maps:new(),
    Map2 = maps:put(producerName, ProducerName, Map),
    Map3 = maps:put(productName, ProductName, Map2),
    Importers = getImporters(lists:nthtail(4, Message)),
    maps:put(sales, Importers, Map3).

% Extract importers who bought something
getImporters(ListImporters) ->
    getImporters(ListImporters, #{}, length(ListImporters)).

getImporters(_, Importers, 0) ->
    Importers;
getImporters(ListImporters, Importers, N) ->
    Username = lists:nth(1, ListImporters),
    SaleInfo = lists:nth(2, ListImporters),
    getImporters(lists:nthtail(2, ListImporters), maps:put(Username, SaleInfo, Importers), N-2).


% Sends producer's username and product to each importer and extract them from importers' map
sendImporters(Importers, [], _, _) ->
    Importers;
sendImporters(Importers, [H|T], ProducerName, ProductName) ->
    {Username, SaleInfo} = H,
    {ok, PID} = maps:find(Username, Importers),
    ToSend = io_lib:format("Import:~p:~p:~p~n", [ProducerName, ProductName, SaleInfo]),
    PID ! {producer, negotiatorsHandler, ToSend},
    sendImporters(maps:remove(Username, Importers), T, ProducerName, ProductName).
