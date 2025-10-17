%%%-------------------------------------------------------------------
%%% @doc
%%% OTP Application Callback Module
%%% Starts the supervisor tree and HTTP server
%%% @end
%%%-------------------------------------------------------------------
-module(switchback_chat_app).
-behaviour(application).

-export([start/2, stop/1]).

-define(PORT, 8000).

start(_StartType, _StartArgs) ->
    io:format("🟣 Starting Switchback Chat Application...~n"),

    % Start the supervisor (which starts chat_room)
    {ok, Pid} = switchback_chat_sup:start_link(),

    % Start Cowboy HTTP server
    start_http_server(),

    {ok, Pid}.

stop(_State) ->
    ok.

start_http_server() ->
    % Define routes
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/dist/[...]", static_handler, []},
            {"/ws", ws_handler, []},
            {"/api/stats", stats_handler, []},
            {"/api/users", users_handler, []},
            {'_', page_handler, []}
        ]}
    ]),

    % Start Cowboy HTTP server
    {ok, _} = cowboy:start_clear(http_listener,
        [{port, ?PORT}],
        #{env => #{dispatch => Dispatch}}
    ),

    io:format("🟣 Erlang chat server listening on http://0.0.0.0:~p~n", [?PORT]),
    io:format("   Real-time WebSocket chat powered by OTP!~n"),
    io:format("   Actor model: Each user is a process~n"),
    io:format("   Fault tolerance: Supervised processes that restart on crash~n~n"),
    ok.
