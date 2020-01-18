-module(server).

-export([server/0]).

-import(login, [handler/1]).
-import(client, [client/1]).
-import(negotiator, [negotiator/1]).

-include("protos.hrl").

% Inicialização do servidor
server() ->
    % Inicia o handler para os users, o loginHandler para controlo de acessos de clientes e o negotiatorsHandler para o tratar dos negociadores
    register(server, self()),
    register(loginHandler, spawn(fun() -> handler(#{}) end)),
    % Abre o socket
    {ok, LSock} = gen_tcp:listen(1234, [binary, {packet, line}, {reuseaddr, true}]),
    negotiatorsConnect(LSock, #{}, 5),
    io:format("Server started~n", []),
    acceptor(LSock).

% Ligações ao servidor
acceptor(LSock) ->
    % Aceita ligações
    {ok, Sock} = gen_tcp:accept(LSock),
    io:format("New connection~n", []),
    spawn(fun() -> acceptor(LSock) end),
    % Nova conexão
    client(Sock).

negotiatorsConnect(_, Negotiators, 0) ->
    register(negotiatorsHandler, spawn(fun() -> negotiators(#{}, Negotiators, 0) end));

negotiatorsConnect(LSock, Negotiators, N) ->
    % estabelecer conexão ao socket do negociador
    {ok, Sock} = gen_tcp:accept(LSock),
    PID = spawn(fun() -> negotiators(Sock, #{}, #{}) end),
    negotiatorsConnect(LSock, Negotiators ++ [PID], N-1).

negotiators(Map, Negotiators, N) ->
    receive
        {new_producer, Product, Username, PID, Data} ->
            case maps:find(Product, Map) of
                {ok, Value} ->
                    Value ! {new_producer, Username, PID, Data};
                _ ->
                    NewMap = maps:put(Product, lists:nth(Negotiators, N), Map),
                    lists:nth(Negotiators, N) ! {new_producer, Username, PID, Data},
                    negotiators(NewMap, Negotiators, N rem length(Negotiators))
            end;
        {new_importer, Product, Username, PID, Data} ->
            case maps:find(Product, Map) of
                {ok, Value} ->
                    Value ! {new_importer, Username, PID, Data};
                _ ->
                    NewMap = maps:put(Product, lists:nth(Negotiators, N), Map),
                    lists:nth(Negotiators, N) ! {new_importer, Username, PID, Data},
                    negotiators(NewMap, Negotiators, N rem length(Negotiators))
            end
    end.
