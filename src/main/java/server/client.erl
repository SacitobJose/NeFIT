-module(client).
-export([client/1]).
-import(login, [create_account/2, cancel_register/2, login/2, logout/1]).
-import(importer, [importer/2]).
-import(producer, [producer/2]).

client(Sock) ->


-module(client).
-export([client/1]).
-import(login, [create_account/2, cancel_register/2, login/2, logout/1]).
-import(importer, [importer/2]).
-import(producer, [producer/2]).

-include("protos.hrl").

client(Sock) ->
    L = gen_tcp:recv(Sock, 4),
    Data = gen_tcp:recv(Sock, L),
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
    end
    client(Sock, Username).