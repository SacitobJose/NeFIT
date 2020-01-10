-module(login).
-export([create_account/2, cancel_register/2, login/2, logout/1, handler/1]).

create_account(User, Pass) ->
    loginHandler ! {register, User, Pass, self()},
    receive
        {loginHandler, Res} ->
            Res
    end.

cancel_register(User, Pass) ->
    loginHandler ! {close_account, User, Pass, self()},
    receive
        {loginHandler, Res} ->
            Res
    end.

login(User, Pass) ->
    loginHandler ! {login, User, Pass, self()},
    receive
        {loginHandler, Res} ->
            Res
    end.

logout(User) ->
    loginHandler ! {logout, User, self()},
    receive
        {loginHandler, Res} ->
            Res
    end.

handler(Map) ->
    receive
        {register, User, Pass, From} ->
            case maps:find(User, Map) of
                {ok, _} ->
                    From ! {loginHandler, user_exists},
                    handler(Map);
                error ->
                    From ! {loginHandler, ok},
                    V = {Pass, false},
                    Map1 = maps:put(User, V, Map),
                    handler(Map1)
            end;

        {cancel_register, User, Pass, From} ->
            case maps:find(User, Map) of
                {ok, {P, _}} ->
                    case Pass of
                        P ->
                            From ! {loginHandler, ok},
                            Map1 = maps:remove(User, Map),
                            handler(Map1);
                        _ ->
                            From ! {loginHandler, wrong_password},
                            handler(Map)
                    end;
                error ->
                    From ! {loginHandler, wrong_user},
                    handler(Map)
            end;

        {login, User, Pass, From} ->
            case maps:find(User, Map) of
                {ok, {P, L}} ->
                    case {Pass, L} of
                        {P, false} ->
                            From ! {loginHandler, ok},
                            Map1 = maps:update(User, {Pass, true}, Map),
                            handler(Map1);
                        {P, true} ->
                            From ! {loginHandler, already_loggedin},
                            handler(Map); 
                        _ ->
                            From ! {loginHandler, wrong_password},
                            handler(Map)
                    end;
                error ->
                    From ! {loginHandler, wrong_user},
                    handler(Map)
            end;

        {logout, User, From} ->
            case maps:find(User, Map) of
                {ok, {P, _}} ->
                    From ! {loginHandler, ok},
                    Map1 = maps:update(User, {P, false}, Map),
                    handler(Map1);
                error ->
                    From ! {loginHandler, wrong_user}
            end
    end.