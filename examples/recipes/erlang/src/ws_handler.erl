%%%-------------------------------------------------------------------
%%% @doc
%%% WebSocket Handler - Each connection is a separate actor/process
%%% Demonstrates Erlang's lightweight process model
%%% @end
%%%-------------------------------------------------------------------
-module(ws_handler).
-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

-record(state, {
    username = undefined,
    joined = false
}).

%%====================================================================
%% Cowboy WebSocket Callbacks
%%====================================================================

init(Req, _State) ->
    {cowboy_websocket, Req, #state{}}.

websocket_init(State) ->
    io:format("🟣 New WebSocket connection established~n"),
    {ok, State}.

websocket_handle({text, RawMsg}, State) ->
    % Parse JSON message
    case jsx:decode(RawMsg, [return_maps]) of
        #{<<"type">> := <<"join">>, <<"username">> := Username} ->
            handle_join(binary_to_list(Username), State);

        #{<<"type">> := <<"message">>, <<"message">> := Message} ->
            handle_message(binary_to_list(Message), State);

        #{<<"type">> := <<"leave">>} ->
            handle_leave(State);

        _ ->
            {ok, State}
    end;

websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info({chat_event, Event}, State) ->
    % Forward chat events to WebSocket client
    Json = jsx:encode(Event),
    {reply, {text, Json}, State};

websocket_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _Req, #state{username = Username, joined = true}) ->
    chat_room:leave(self()),
    io:format("🟣 WebSocket disconnected (was: ~s)~n", [Username]),
    ok;

terminate(_Reason, _Req, _State) ->
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

handle_join(Username, State) ->
    case chat_room:join(Username, self()) of
        {ok, Users} ->
            io:format("🟣 WebSocket user joined: ~s~n", [Username]),

            Response = jsx:encode(#{
                type => join_success,
                username => list_to_binary(Username),
                users => [list_to_binary(U) || U <- Users]
            }),

            NewState = State#state{username = Username, joined = true},
            {reply, {text, Response}, NewState};

        {error, username_taken} ->
            Response = jsx:encode(#{
                type => error,
                message => <<"Username already taken">>
            }),

            {reply, {text, Response}, State}
    end.

handle_message(Message, #state{username = Username, joined = true} = State) ->
    chat_room:send_message(Username, Message),
    {ok, State};

handle_message(_Message, State) ->
    % Not joined yet, ignore
    {ok, State}.

handle_leave(#state{joined = true} = State) ->
    chat_room:leave(self()),
    {stop, State};

handle_leave(State) ->
    {stop, State}.
