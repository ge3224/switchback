%%%-------------------------------------------------------------------
%%% @doc
%%% Chat Room Manager - Actor Model Implementation
%%% Manages all chat users and message broadcasting
%%% Each user is a separate Erlang process (actor)
%%% @end
%%%-------------------------------------------------------------------
-module(chat_room).
-behaviour(gen_server).

%% API
-export([start_link/0, join/2, leave/1, send_message/2, get_users/0, get_stats/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    users = #{},      % #{Username => Pid}
    pids = #{},       % #{Pid => Username}
    messages = [],    % Message history
    stats = #{
        total_messages => 0,
        total_joins => 0,
        uptime_start => erlang:system_time(second)
    }
}).

%%====================================================================
%% API Functions
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Join chat room
join(Username, Pid) ->
    gen_server:call(?MODULE, {join, Username, Pid}).

%% Leave chat room
leave(Pid) ->
    gen_server:cast(?MODULE, {leave, Pid}).

%% Send message to all users
send_message(Username, Message) ->
    gen_server:cast(?MODULE, {message, Username, Message}).

%% Get current users
get_users() ->
    gen_server:call(?MODULE, get_users).

%% Get statistics
get_stats() ->
    gen_server:call(?MODULE, get_stats).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init([]) ->
    io:format("🟣 Chat room initialized~n"),
    {ok, #state{}}.

handle_call({join, Username, Pid}, _From, State) ->
    #state{users = Users, pids = Pids, stats = Stats} = State,

    % Check if username is taken
    case maps:is_key(Username, Users) of
        true ->
            {reply, {error, username_taken}, State};
        false ->
            % Monitor the user process
            erlang:monitor(process, Pid),

            % Update state
            NewUsers = maps:put(Username, Pid, Users),
            NewPids = maps:put(Pid, Username, Pids),
            NewStats = maps:put(total_joins, maps:get(total_joins, Stats) + 1, Stats),

            io:format("🟣 ~s joined the chat (PID: ~p)~n", [Username, Pid]),

            % Broadcast join event to all other users
            JoinMsg = #{
                type => user_joined,
                username => list_to_binary(Username),
                timestamp => erlang:system_time(second),
                user_count => maps:size(NewUsers)
            },
            broadcast(JoinMsg, NewUsers, Pid),

            NewState = State#state{
                users = NewUsers,
                pids = NewPids,
                stats = NewStats
            },

            {reply, {ok, maps:keys(NewUsers)}, NewState}
    end;

handle_call(get_users, _From, State) ->
    Users = maps:keys(State#state.users),
    {reply, {ok, Users}, State};

handle_call(get_stats, _From, State) ->
    #state{users = Users, stats = Stats} = State,
    Uptime = erlang:system_time(second) - maps:get(uptime_start, Stats),

    StatsMap = #{
        online_users => maps:size(Users),
        total_messages => maps:get(total_messages, Stats),
        total_joins => maps:get(total_joins, Stats),
        uptime_seconds => Uptime
    },

    {reply, {ok, StatsMap}, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({message, Username, Message}, State) ->
    #state{users = Users, messages = Messages, stats = Stats} = State,

    Timestamp = erlang:system_time(second),

    % Create message object - ensure strings are binaries
    MsgObj = #{
        type => chat_message,
        username => list_to_binary(Username),
        message => list_to_binary(Message),
        timestamp => Timestamp
    },

    % Broadcast to all users
    broadcast(MsgObj, Users, undefined),

    % Update message history (keep last 100)
    NewMessages = lists:sublist([MsgObj | Messages], 100),
    NewStats = maps:put(total_messages, maps:get(total_messages, Stats) + 1, Stats),

    io:format("🟣 Message from ~s: ~s~n", [Username, Message]),

    NewState = State#state{
        messages = NewMessages,
        stats = NewStats
    },

    {noreply, NewState};

handle_cast({leave, Pid}, State) ->
    NewState = remove_user(Pid, State),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    % User process died - remove them
    io:format("🟣 User process ~p died, removing from chat~n", [Pid]),
    NewState = remove_user(Pid, State),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

broadcast(Message, Users, ExcludePid) ->
    maps:foreach(fun(_Username, Pid) ->
        case Pid of
            ExcludePid -> ok;  % Don't send to self
            _ -> Pid ! {chat_event, Message}
        end
    end, Users).

remove_user(Pid, State) ->
    #state{users = Users, pids = Pids} = State,

    case maps:find(Pid, Pids) of
        {ok, Username} ->
            io:format("🟣 ~s left the chat~n", [Username]),

            % Remove from maps
            NewUsers = maps:remove(Username, Users),
            NewPids = maps:remove(Pid, Pids),

            % Broadcast leave event
            LeaveMsg = #{
                type => user_left,
                username => list_to_binary(Username),
                timestamp => erlang:system_time(second),
                user_count => maps:size(NewUsers)
            },
            broadcast(LeaveMsg, NewUsers, undefined),

            State#state{users = NewUsers, pids = NewPids};
        error ->
            State
    end.
