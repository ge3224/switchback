%%%-------------------------------------------------------------------
%%% @doc
%%% Users API Handler - Returns list of online users
%%% @end
%%%-------------------------------------------------------------------
-module(users_handler).
-export([init/2]).

init(Req, State) ->
    {ok, Users} = chat_room:get_users(),

    UsersBinary = [list_to_binary(U) || U <- Users],

    Response = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(#{users => UsersBinary}),
        Req
    ),

    {ok, Response, State}.
