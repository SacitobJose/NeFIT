-module(client).
-export([client/1]).
-import(login, [create_account/2, cancel_register/2, login/2, logout/1]).
-import(importer, [importer/2]).
-import(producer, [producer/2]).

client(Sock) ->
    % recebe uma conexão, verifica a autenticação e chama importer(Sock, Username) ou producer(Sock, Username, TimeOut)
    receive
        {tcp, _, Data} ->
            Message = string:split(string:trim(Data), "_", all),
            Fazer = lists:nth(2, Message),

            case Fazer of
                <<"0">> ->
                    case length(Message) of
                        5 ->
                            Kind = lists:nth(3, Message),
                            Username = lists:nth(4, Message),
                            Password = lists:nth(5, Message),
                            Res = login(Username, Password),
                            case Res of
                                ok ->
                                    gen_tcp:send(Sock, io_lib:format("ServerResponse_1~n", [])),
                                    case Kind of
                                        <<"0">> ->
                                            producer(Sock, Username);
                                        <<"1">> ->
                                            importer(Sock, Username)
                                    end;
                                wrong_user ->
                                    gen_tcp:send(Sock, io_lib:format("ServerResponse_0~n", [])),
                                    client(Sock);
                                already_loggedin ->
                                    gen_tcp:send(Sock, io_lib:format("ServerResponse_0~n", [])),
                                    client(Sock);
                                wrong_password ->
                                    gen_tcp:send(Sock, io_lib:format("ServerResponse_0~n", [])),
                                    client(Sock)
                            end;
                        _ ->
                            gen_tcp:send(Sock, io_lib:format("ServerResponse_0~n", [])),
                            client(Sock)
                    end;
                <<"1">> ->
                    case length(Message) of
                        5 ->
                            Kind = lists:nth(3, Message),
                            Username = lists:nth(4, Message),
                            Password = lists:nth(5, Message),
                            Res = create_account(Username, Password),
                            case Res of
                                ok ->
                                    gen_tcp:send(Sock, io_lib:format("ServerResponse_1~n", [])),
                                    io:format("New Register~n", []),
                                    case Kind of
                                        <<"0">> ->
                                            producer(Sock, Username);
                                        <<"1">> ->
                                            importer(Sock, Username)
                                    end;
                                user_exists ->
                                    gen_tcp:send(Sock, io_lib:format("ServerResponse_0~n", [])),
                                    client(Sock)
                            end;
                        _ ->
                            gen_tcp:send(Sock, io_lib:format("ServerResponse_0~n", [])),
                            client(Sock)
                    end
            end;
        {tcp_closed, _} ->
            io:format("user disconnected ~n");
        {tcp_error, _, _} ->
            io:format("user disconnected due to error ~n")
    end.