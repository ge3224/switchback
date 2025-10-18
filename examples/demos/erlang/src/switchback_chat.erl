%%%-------------------------------------------------------------------
%%% @doc
%%% Switchback Chat - Erlang/OTP Recipe
%%% Demonstrates the Actor Model with real-time WebSocket chat
%%% Features: OTP supervision, message passing, fault tolerance
%%% @end
%%%-------------------------------------------------------------------
-module(switchback_chat).
-export([start/0, start_link/0, init/1, handle/2, handle_event/3, terminate/3]).

-define(PORT, 8000).

%%% Application entry point
start() ->
    application:ensure_all_started(cowboy),
    io:format("🟣 Starting Switchback Chat Server...~n"),

    % Start the chat room manager
    {ok, ChatPid} = chat_room:start_link(),
    io:format("🟣 Chat room manager started (PID: ~p)~n", [ChatPid]),

    % Define routes
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", cowboy_static, {priv_file, switchback_chat, "index.html"}},
            {"/dist/[...]", cowboy_static, {dir, "dist"}},
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

    receive
        stop -> ok
    end.

start_link() ->
    Pid = spawn_link(fun() -> start() end),
    {ok, Pid}.

init(_Opts) ->
    {ok, undefined}.

handle(Req, State) ->
    {ok, Req, State}.

handle_event(_EventName, _Event, State) ->
    {ok, State}.

terminate(_Reason, _Req, _State) ->
    ok.
