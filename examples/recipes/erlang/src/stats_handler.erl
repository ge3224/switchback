%%%-------------------------------------------------------------------
%%% @doc
%%% Stats API Handler - Returns chat statistics
%%% @end
%%%-------------------------------------------------------------------
-module(stats_handler).
-export([init/2]).

init(Req, State) ->
    {ok, Stats} = chat_room:get_stats(),

    Response = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        jsx:encode(Stats),
        Req
    ),

    {ok, Response, State}.
