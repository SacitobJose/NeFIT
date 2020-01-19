-module(negotiator).

-export([negotiator/3]).

-include("protos.hrl").

negotiator(Sock, Importers, Producers) ->
    io:format("Negotiator actor initiated~n", []),
    receive
      {new_producer, Username, PID, Data} ->
	  gen_tcp:send(Sock, io_lib:format("Producer:~p~n",[Data])),
	  io:format("Novo produtor enviado~n", []),
	  NewProducers = maps:put(Username, PID, Producers),
	  negotiator(Sock, Importers, NewProducers);
      {new_importer, Username, PID, Data} ->
	  gen_tcp:send(Sock, io_lib:format("Importer:~p~n",[Data])),
	  NewImporters = maps:put(Username, PID, Importers),
	  negotiator(Sock, NewImporters, Producers);
      {tcp, _, Data} ->
	  io:format("Negociação concluída~n", []),
	  % decode data
	  Map_Data = decode(Data),
	  % send info to producer
	  {ok, ProducerName} = maps:find(producerName, Map_Data),
	  {ok, Producer} = maps:find(ProducerName, Producers),
	  Protobuf = maps:find(protobuf, Map_Data),
	  Producer !
	    {timeout, negotiatorsHandler,
	     io_lib:format("Timeout:~p~n", Protobuf)},
	  % get product name
	  {ok, ProductName} = maps:find(productName, Map_Data),
	  % extract importers' usernames, send them the producer's info and remove them from their map
	  {ok, ImportersBuying} = maps:find(sales, Map_Data),
	  NewImporters = sendImporters(Importers,
				       maps:to_list(ImportersBuying),
				       ProducerName, ProductName),
	  negotiator(Sock, NewImporters,
		     maps:remove(ProducerName, Producers))
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
    Map4 = maps:put(sales, Importers, Map3),
    maps:put(protobuf,
	     lists:nthtail(length(Message) - 1, Message), Map4).

% Extract importers who bought something
getImporters(ListImporters) ->
	io:format("~p~n", [length(ListImporters)]),
    getImporters(ListImporters, #{}, length(ListImporters)).

getImporters(_, Importers, 0) -> Importers;
getImporters(ListImporters, Importers, N) ->
    Username = lists:nth(1, ListImporters),
    SaleInfo = lists:nth(2, ListImporters),
    getImporters(lists:nthtail(2, ListImporters),
		 maps:put(Username, SaleInfo, Importers), N - 2).

% Sends producer's username and product to each importer and extract them from importers' map
sendImporters(Importers, [], _, _) -> Importers;
sendImporters(Importers, [H | T], ProducerName,
	      ProductName) ->
    {Username, SaleInfo} = H,
    {ok, PID} = maps:find(Username, Importers),
    ToSend = io_lib:format("Import:~p:~p:~p~n",
			   [ProducerName, ProductName, SaleInfo]),
    PID ! {producer, negotiatorsHandler, ToSend},
    sendImporters(maps:remove(Username, Importers), T,
		  ProducerName, ProductName).
