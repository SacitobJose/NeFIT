-module(client).
-export([client/1]).
-import(login, [create_account/2, cancel_register/2, login/2, logout/1]).
-import(importer, [importer/2]).
-import(producer, [producer/2]).

-include("protos.hrl").

client(Sock) ->
    % recebe uma conexão, verifica a autenticação e chama importer(Sock, Username) ou producer(Sock, Username, TimeOut)
    receive
        {tcp, _, Data} ->
            Auth = protos:decode_msg(Data, 'Authentication'),
            case maps:find(type, Auth) of
                "LOGIN" ->
                    Username = maps:find(username, Auth),
                    Password = maps:find(password, Auth),
                    Kind = maps:find(kind, Auth),
                        Res = login(Username, Password),
                        case Res of
                            ok ->
                                ServerResponse = protos:encode_msg(#'ServerResponse'{success = true}),
                                gen_tcp:send(Sock, ServerResponse),
                                case Kind of
                                    <<"Producer">> ->
                                        producer(Sock, Username);
                                    <<"Importer">> ->
                                        importer(Sock, Username)
                                end;
                            wrong_user ->
                                ServerResponse = protos:encode_msg(#'ServerResponse'{success = false}),
                                gen_tcp:send(Sock, ServerResponse),
                                client(Sock);
                            already_loggedin ->
                                ServerResponse = protos:encode_msg(#'ServerResponse'{success = false}),
                                gen_tcp:send(Sock, ServerResponse),
                                client(Sock);
                            wrong_password ->
                                ServerResponse = protos:encode_msg(#'ServerResponse'{success = false}),
                                gen_tcp:send(Sock, ServerResponse),
                                client(Sock)
                        end;
                "REGISTER" ->
                    Username = maps:find(username, Auth),
                    Password = maps:find(password, Auth),
                    Kind = maps:find(kind, Auth),
                        Res = create_account(Username, Password),
                        case Res of
                            ok ->
                                ServerResponse = protos:encode_msg(#'ServerResponse'{success = true}),
                                gen_tcp:send(Sock, ServerResponse),
                                case Kind of
                                    <<"Producer">> ->
                                        producer(Sock, Username);
                                    <<"Importer">> ->
                                        importer(Sock, Username)
                                end;
                            user_exists ->
                                ServerResponse = protos:encode_msg(#'ServerResponse'{success = false}),
                                gen_tcp:send(Sock, ServerResponse),
                                client(Sock)
                        end
            end;
        {tcp_closed, _} ->
            io:format("user disconnected ~n");
        {tcp_error, _, _} ->
            io:format("user disconnected due to error ~n")
    end.