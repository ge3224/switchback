%%%-------------------------------------------------------------------
%%% @doc
%%% OTP Supervisor - Demonstrates fault tolerance
%%% If article_manager crashes, it will be automatically restarted
%%% "Let it crash" philosophy in action!
%%% @end
%%%-------------------------------------------------------------------
-module(switchback_chat_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    io:format("🟣 Starting OTP supervisor~n"),

    SupFlags = #{
        strategy => one_for_one,  % Restart only the failed process
        intensity => 10,          % Max 10 restarts
        period => 60              % Within 60 seconds
    },

    ChildSpecs = [
        #{
            id => article_manager,
            start => {article_manager, start_link, []},
            restart => permanent,  % Always restart if it crashes
            shutdown => 5000,
            type => worker,
            modules => [article_manager]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.
