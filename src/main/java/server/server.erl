-module(server).

-export([server/0]).

-import(login, [handler/1]).
-import(client, [client/1]).
-import(negotiator, [negotiator/3]).

% Inicialização do servidor
server() ->
    % Inicia o handler para os users, o loginHandler para controlo de acessos de clientes e o negotiatorsHandler para o tratar dos negociadores
    register(server, self()),
    register(loginHandler, spawn(fun() -> handler(#{}) end)),
    % Abre o socket
    {ok, LSock} = gen_tcp:listen(1234, [binary, {packet, raw}, {reuseaddr, true}, {active, false}]),
    io:format("Server started~n", []),
    negotiatorsConnect(LSock, [], 1).

% Ligações ao servidor
acceptor(LSock) ->
    % Aceita ligações
    {ok, Sock} = gen_tcp:accept(LSock),
    io:format("New connection~n", []),
    spawn(fun() -> acceptor(LSock) end),
    % Nova conexão
    client(Sock).

negotiatorsConnect(LSock, Negotiators, 0) ->
    register(negotiatorsHandler, spawn(fun() -> negotiators(#{}, Negotiators, 1) end)),
    acceptor(LSock);

negotiatorsConnect(LSock, Negotiators, N) ->
    % estabelecer conexão ao socket do negociador
    {ok, Sock} = gen_tcp:accept(LSock),
    io:format("Negotiator connected~n", []),
    PID = self(),
    spawn(fun() -> negotiatorsConnect(LSock, Negotiators ++ [PID], N-1) end),
    negotiator(Sock, #{}, #{}).

negotiators(Map, Negotiators, N) ->
    receive
        {new_producer, Username, PID, Product, Data} ->
            case maps:find(Product, Map) of
                {ok, Value} ->
                    Value ! {new_producer, Username, PID, Product, Data},
                    negotiators(Map, Negotiators, N);
                _ ->
                    NewMap = maps:put(Product, lists:nth(N, Negotiators), Map), 
                    lists:nth(N, Negotiators) ! {new_producer, Username, PID, Product, Data},
                    negotiators(NewMap, Negotiators, (N rem length(Negotiators)) + 1)
            end;
        {new_importer, Username, PID, Product, Data} ->
            case maps:find(Product, Map) of
                {ok, Value} ->
                    Value ! {new_importer, Username, PID, Product, Data},
                    negotiators(Map, Negotiators, N);
                _ ->
                    NewMap = maps:put(Product, lists:nth(N, Negotiators), Map),
                    lists:nth(N, Negotiators) ! {new_importer, Username, PID, Product, Data},
                    negotiators(NewMap, Negotiators, (N rem length(Negotiators)) + 1)
            end
    end.
