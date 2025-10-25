%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @doc
%%% OTP Application Callback Module
%%% Starts the supervisor tree and HTTP server for the Erlang blog demo
%%% @end
%%%-------------------------------------------------------------------
-module(switchback_blog_app).
-behaviour(application).

-export([start/2, stop/1]).

-define(PORT, 8000).

start(_StartType, _StartArgs) ->
    io:format("🟣 Starting Switchback Blog Application...~n"),

    % Start the supervisor (which starts article_manager)
    {ok, Pid} = switchback_blog_sup:start_link(),

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
            {'_', page_handler, []}
        ]}
    ]),

    % Start Cowboy HTTP server
    {ok, _} = cowboy:start_clear(http_listener,
        [{port, ?PORT}],
        #{env => #{dispatch => Dispatch}}
    ),

    io:format("🟣 Erlang blog server listening on http://0.0.0.0:~p~n", [?PORT]),
    io:format("   Server-side rendering powered by OTP!~n"),
    io:format("   Actor model: Articles managed by gen_server~n"),
    io:format("   Fault tolerance: Supervised processes that restart on crash~n~n"),
    ok.
