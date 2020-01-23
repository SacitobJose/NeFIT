-module(client).
-export([client/1]).
-import(login, [create_account/2, cancel_register/2, login/2, logout/1]).
-import(importer, [importer/2]).
-import(producer, [producer/2]).

-include("protos.hrl").

client(Sock) ->
    {ok, Length} = gen_tcp:recv(Sock, 4),
    {ok, Data} = gen_tcp:recv(Sock, binary:decode_unsigned(Length)),
    {_, Tipo, TipoCliente, Username, Password} = protos:decode_msg(Data, 'Authentication'),
    case Tipo of
        'LOGIN' ->
            Res = login(Username, Password),
            case Res of
                ok ->
                    ServerResponse = protos:encode_msg(#'ServerResponse'{success = true}),
                    X = binary:encode_unsigned(byte_size(ServerResponse)),
                    Response = <<X/binary, ServerResponse/binary>>,
                    gen_tcp:send(Sock, Response),
                    case TipoCliente of
                        'PRODUCER' ->
                            producer(Sock, Username);
                        'IMPORTER' ->
                            importer(Sock, Username)
                    end;
                wrong_user ->
                    ServerResponse = protos:encode_msg(#'ServerResponse'{success = false}),
                    X = binary:encode_unsigned(byte_size(ServerResponse)),
                    Response = <<X/binary, ServerResponse/binary>>,
                    gen_tcp:send(Sock, Response),
                    client(Sock);
                already_loggedin ->
                    ServerResponse = protos:encode_msg(#'ServerResponse'{success = false}),
                    X = binary:encode_unsigned(byte_size(ServerResponse)),
                    Response = <<X/binary, ServerResponse/binary>>,
                    gen_tcp:send(Sock, Response),
                    client(Sock);
                wrong_password ->
                    ServerResponse = protos:encode_msg(#'ServerResponse'{success = false}),
                    X = binary:encode_unsigned(byte_size(ServerResponse)),
                    Response = <<X/binary, ServerResponse/binary>>,
                    gen_tcp:send(Sock, Response),
                    client(Sock)
            end;
        'REGISTER' ->
            Res = create_account(Username, Password),
            case Res of
                ok ->
                    ServerResponse = protos:encode_msg(#'ServerResponse'{success = true}),
                    X = binary:encode_unsigned(byte_size(ServerResponse)),
                    Response = <<X/binary, ServerResponse/binary>>,
                    gen_tcp:send(Sock, Response),
                    case TipoCliente of
                        'PRODUCER' ->
                            producer(Sock, Username);
                        'IMPORTER' ->
                            importer(Sock, Username)
                    end;
                user_exists ->
                    ServerResponse = protos:encode_msg(#'ServerResponse'{success = false}),
                    X = binary:encode_unsigned(byte_size(ServerResponse)),
                    Response = <<X/binary, ServerResponse/binary>>,
                    gen_tcp:send(Sock, Response),
                    client(Sock)
            end
    end.
