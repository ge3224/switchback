# Erlang Demo - Switchback SSR Integration

A real-time chat application showcasing **Erlang/OTP's Actor Model** with **Server-Side Rendering (SSR)**. Experience true message-passing concurrency, fault tolerance, and instant first-paint rendering - combining the best of both worlds!

## 🚀 What Makes This Demo Special?

This demo demonstrates **Server-Side Rendering with HTML Morphing** - a unique approach where:

1. **Erlang renders complete HTML** for each page (not just JSON)
2. **Switchback morphs the HTML** into the DOM efficiently
3. **No client-side component code** - the server controls presentation
4. **Instant first paint** - users see content before JavaScript loads
5. **Smooth navigation** - Switchback morphs updates without flickering

This is similar to **Phoenix LiveView** or **HTMX**, but without WebSocket overhead for navigation. Regular HTTP requests deliver server-rendered HTML that Switchback seamlessly morphs into place.

## What's Included

- **src/switchback_chat.erl** - Main application entry point
- **src/chat_room.erl** - Chat room manager using gen_server (Actor Model)
- **src/ws_handler.erl** - WebSocket handler (each connection is a process)
- **src/switchback_chat_sup.erl** - OTP supervisor for fault tolerance
- **src/html_renderer.erl** - **NEW!** Server-side HTML rendering module
- **src/page_handler.erl** - Returns HTML fragments for Switchback morphing
- **app.ts** - Minimal client (90% smaller!) - just Switchback + WebSocket
- **vite.config.ts** - Bundles app.ts + Switchback into single JS file
- **Docker setup** - Multi-stage build with Erlang/OTP 26

## How SSR Works in This Demo

### Traditional Switchback Flow (Other Demos)
```
Browser → Erlang → JSON {component, props} → JS renders HTML
```

### New SSR Flow (This Demo)
```
Browser → Erlang → JSON {component, props, html} → Switchback morphs HTML
```

### The Magic: HTML Morphing

1. **Initial Page Load**:
   ```erlang
   % page_handler.erl renders full HTML
   render_html(PageData) ->
       Html = maps:get(html, PageData),
       [<<"<!DOCTYPE html>...">>,
        <<"<div data-swbk-app>">>,
        Html,  % Server-rendered content!
        <<"</div>...</html>">>].
   ```

2. **Navigation (Switchback Request)**:
   ```erlang
   % Server returns JSON with HTML
   #{
       component => <<"Home">>,
       props => #{stats => ...},
       url => <<"/">>,
       html => html_renderer:render_page(<<"Home">>, Props)
   }
   ```

3. **Client Morphing**:
   ```typescript
   // Switchback's built-in morphElement() efficiently updates DOM
   if (page.html) {
       morphElement(oldElement, newElement);  // Preserves focus, scroll, etc.
   }
   ```

### Benefits of This Approach

✅ **Instant First Paint** - Content visible before JS loads
✅ **Better SEO** - Search engines see real HTML
✅ **Works Without JS** - Graceful degradation
✅ **Smaller Client Bundle** - 90% less JavaScript (from 840 lines → 120 lines!)
✅ **Server Controls UI** - Easier to maintain consistency
✅ **Efficient Updates** - Morphing reuses DOM nodes (preserves input focus, scroll position)
✅ **Progressive Enhancement** - Enhance with JS, work without it

## What is the Actor Model?

The Actor Model is a concurrency paradigm where independent processes (actors) communicate only by sending messages. Unlike threads with shared memory, actors are isolated and communicate asynchronously.

### Erlang's Implementation:

- **Lightweight Processes**: Spawn millions of processes with tiny memory footprint (~2KB each)
- **Message Passing**: Processes communicate via mailboxes, no shared state
- **Fault Tolerance**: "Let it crash" - supervisors restart failed processes
- **Location Transparency**: Actors can be local or on remote nodes

### Real-World Analogy:

Think of actors like people in an office:
- Each person (process) has their own desk and inbox
- They send memos (messages) to each other
- If someone is out sick (crashes), their supervisor assigns a replacement
- No one shares a desk (no shared memory)

### Erlang vs Other Models:

| Feature | Erlang Actors | JavaScript async/await | Go Goroutines |
|---------|---------------|------------------------|---------------|
| **Concurrency** | Millions of processes | Single thread + event loop | Thousands of goroutines |
| **Communication** | Message passing | Callbacks/Promises | Channels + shared memory |
| **Fault Tolerance** | Built-in supervision | Manual error handling | Manual recovery |
| **Isolation** | Complete isolation | Shared global state | Shared memory with locks |
| **Use Case** | Distributed systems, chat, telecom | I/O-bound web apps | CPU-bound tasks |

## How This Demo Works

1. **OTP Supervisor** starts the chat room gen_server
2. **User connects via WebSocket** → New Erlang process spawned
3. **User joins chat** → Registered with chat_room process
4. **User sends message** → Message passed to chat_room
5. **chat_room broadcasts** → Sends message to all user processes via `!` operator
6. **Each user process** → Forwards to WebSocket client
7. **User disconnects/crashes** → Process dies, supervisor cleans up

**The magic**: Each user is a completely independent process. If one user's process crashes, it doesn't affect others!

## Running Locally

### Prerequisites

- Erlang/OTP 26+ ([Install guide](https://www.erlang.org/downloads))
- rebar3 ([Install guide](https://rebar3.org/docs/getting-started/))
- Node.js 20+ with pnpm
- Or just Docker!

### Option 1: Native Build

Build the client app:

```bash
cd examples/demos/erlang

# Install dependencies (uses parent's node_modules for vite/typescript)
pnpm install --dir ../../../

# Build app.ts + Switchback into dist/app.js
pnpm build
```

Build and run the Erlang server:

```bash
# Fetch Erlang dependencies
rebar3 get-deps

# Compile Erlang code
rebar3 compile

# Start the Erlang shell with the application
rebar3 shell
```

Inside the Erlang shell:

```erlang
% Start the application
switchback_chat:start().
```

Open http://localhost:8000

**For development with auto-rebuild:**

```bash
# Terminal 1: Watch and rebuild client on changes
pnpm dev

# Terminal 2: Run Erlang with auto-reload
rebar3 auto --apps switchback_chat
```

### Option 2: Docker (Recommended)

Docker will automatically build everything:

```bash
# From examples/demos/erlang directory
docker-compose up

# Or build first, then run
docker-compose build
docker-compose up
```

Open http://localhost:8000

**Note:** The Dockerfile uses a multi-stage build:
1. **JS builder stage**: Bundles app.ts with Switchback source into single JS file
2. **Erlang builder stage**: Compiles Erlang code with rebar3
3. **Runtime stage**: Minimal Alpine image with Erlang/OTP runtime

## Try the Real-Time Chat

1. Open http://localhost:8000 in multiple browser windows
2. Click "Join Chat Room"
3. Choose different usernames in each window
4. Send messages and see them appear **instantly** in all windows
5. Watch the online users list update in **real-time**
6. Close a browser tab and see the user disappear from the list
7. Check the server logs to see Erlang processes being created/destroyed!

The beauty: Each browser tab is a separate Erlang process communicating via message passing!

## OTP Architecture

### Supervision Tree

```
switchback_chat_sup (supervisor)
    └── chat_room (gen_server)
            ├── Monitors user process 1
            ├── Monitors user process 2
            └── Monitors user process N
```

If the chat_room crashes, the supervisor **automatically restarts it**. This is the "let it crash" philosophy - don't write defensive code, let supervisors handle failures!

### Process Structure

```
User 1 Browser
    ↓ WebSocket
ws_handler Process 1 ──┐
    ↓ Erlang message    │
                        ├──→ chat_room gen_server ←── Manages all users
ws_handler Process 2 ──┤         ↓
    ↑ Erlang message    │    Broadcasts via !
User 2 Browser          │         ↓
    ↑ WebSocket        ─┘    All processes receive message
```

Each `ws_handler` is a separate process. When a message arrives:
1. Process sends message to `chat_room` via `gen_server:cast`
2. `chat_room` receives it in `handle_cast` callback
3. `chat_room` sends message to all user processes via `Pid ! Message`
4. Each process receives it in `websocket_info` callback
5. WebSocket forwards to browser

### Code Walkthrough

**Creating an actor (Erlang process):**

```erlang
% In ws_handler.erl - when user joins
handle_join(Username, State) ->
    case chat_room:join(Username, self()) of  % self() = current process PID
        {ok, Users} -> ...
```

**Sending a message (async):**

```erlang
% In chat_room.erl - broadcasting to all users
broadcast(Message, Users, ExcludePid) ->
    maps:foreach(fun(_Username, Pid) ->
        Pid ! {chat_event, Message}  % ! is the send operator
    end, Users).
```

**Receiving a message:**

```erlang
% In ws_handler.erl - receiving broadcasted message
websocket_info({chat_event, Event}, State) ->
    Json = jsx:encode(Event),
    {reply, {text, Json}, State}.
```

**Supervision (fault tolerance):**

```erlang
% In switchback_chat_sup.erl
init([]) ->
    ChildSpecs = [
        #{
            id => chat_room,
            start => {chat_room, start_link, []},
            restart => permanent,  % Always restart if it crashes!
            shutdown => 5000,
            type => worker
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.
```

## Key Features Demonstrated

- ✅ **Actor Model** - Each user is an independent process
- ✅ **Message Passing** - Processes communicate via `!` operator
- ✅ **OTP gen_server** - Generic server behavior for chat_room
- ✅ **OTP Supervision** - Automatic restart on crashes
- ✅ **Process Monitoring** - Detect when user processes die
- ✅ **WebSocket per Process** - Each connection is a lightweight process
- ✅ **Real-time Broadcasting** - Efficient message distribution
- ✅ **Fault Isolation** - One crashed user doesn't affect others
- ✅ **Interactive Switchback UI** - Purple theme for Erlang!

## File Structure

```
erlang/
├── src/
│   ├── switchback_chat.erl        # Main application
│   ├── switchback_chat_app.erl    # OTP application callback
│   ├── switchback_chat_sup.erl    # OTP supervisor
│   ├── chat_room.erl              # Chat room gen_server (actor)
│   ├── ws_handler.erl             # WebSocket handler
│   ├── page_handler.erl           # Switchback page routing
│   ├── stats_handler.erl          # Stats API
│   └── users_handler.erl          # Users API
├── app.ts                          # Frontend Switchback app
├── vite.config.ts                  # Vite bundler config
├── package.json                    # Build scripts
├── rebar.config                    # Erlang dependencies
├── Dockerfile                      # Docker image
├── docker-compose.yml              # Docker setup
└── README.md                       # This file
```

## Understanding OTP Behaviors

OTP (Open Telecom Platform) provides battle-tested patterns for building reliable systems.

### gen_server (Generic Server)

A `gen_server` is a standardized server process:

```erlang
-module(chat_room).
-behaviour(gen_server).

% Callbacks you implement:
init/1           % Initialize server state
handle_call/3    % Synchronous requests (caller waits for reply)
handle_cast/2    % Asynchronous requests (fire and forget)
handle_info/2    % Handle any other messages
terminate/2      % Cleanup before shutdown
```

**Why use gen_server?**
- Standardized interface
- Built-in debugging tools
- Hot code reloading support
- OTP supervision integration

### supervisor

A `supervisor` monitors child processes and restarts them on failure:

```erlang
% Restart strategies:
one_for_one     % Restart only the failed process
one_for_all     % Restart all children if one fails
rest_for_one    % Restart failed + all started after it
```

**Why use supervisors?**
- Automatic fault recovery
- Configurable restart strategies
- Prevents cascading failures
- Clear failure boundaries

## Extending This Example

### Add Private Messages

In `chat_room.erl`:

```erlang
handle_cast({private_message, From, To, Message}, State) ->
    #state{users = Users} = State,
    case maps:find(To, Users) of
        {ok, ToPid} ->
            MsgObj = #{type => private_message, from => From, message => Message},
            ToPid ! {chat_event, MsgObj};
        error ->
            ok
    end,
    {noreply, State}.
```

### Add Chat Rooms

Create multiple `chat_room` processes, one per room:

```erlang
% Supervisor spawns multiple chat rooms
rooms() -> [
    {room_1, "General"},
    {room_2, "Random"},
    {room_3, "Tech Talk"}
].

init([]) ->
    ChildSpecs = [
        chat_room_spec(RoomId, RoomName)
        || {RoomId, RoomName} <- rooms()
    ],
    {ok, {SupFlags, ChildSpecs}}.
```

### Add Persistence

Use Mnesia (Erlang's distributed database):

```erlang
% In chat_room.erl
-record(chat_message, {id, username, message, timestamp}).

init([]) ->
    mnesia:create_table(chat_message,
        [{attributes, record_info(fields, chat_message)}]),
    {ok, #state{}}.

handle_cast({message, Username, Message}, State) ->
    % Store in Mnesia
    mnesia:dirty_write(#chat_message{
        id = make_ref(),
        username = Username,
        message = Message,
        timestamp = erlang:system_time(second)
    }),
    % ... broadcast ...
```

### Add Distributed Nodes

Run chat on multiple servers:

```bash
# Node 1
erl -name chat1@server1.com -setcookie mysecret

# Node 2
erl -name chat2@server2.com -setcookie mysecret

# Connect nodes
(chat2@server2.com)> net_adm:ping('chat1@server1.com').
pong

# Now processes can communicate across machines!
```

### Add Rate Limiting

Prevent spam by tracking message frequency:

```erlang
-record(state, {
    users = #{},
    rate_limit = #{}  % #{Username => {Count, WindowStart}}
}).

handle_cast({message, Username, Message}, State) ->
    case check_rate_limit(Username, State) of
        ok ->
            broadcast(Message, State),
            {noreply, update_rate_limit(Username, State)};
        {error, rate_limited} ->
            % Notify user they're rate limited
            {noreply, State}
    end.
```

## Performance Notes

Erlang's process model is incredibly efficient:
- **2KB initial stack** per process (grows as needed)
- **Sub-microsecond context switching**
- **Efficient garbage collection** per-process (not global)
- **Can run millions of processes** on a single machine

WhatsApp handled **3 million concurrent connections per server** with Erlang before Facebook acquisition!

## Testing the Fault Tolerance

Want to see "let it crash" in action?

1. Start the server
2. Open the Erlang shell
3. Find the chat_room process:

```erlang
whereis(chat_room).
% Returns: <0.123.0>
```

4. Kill it manually:

```erlang
exit(whereis(chat_room), kill).
```

5. **Watch it restart automatically!**

```erlang
whereis(chat_room).
% Returns: <0.456.0> (different PID!)
```

The supervisor detected the crash and restarted the process. Users will reconnect automatically!

## Troubleshooting

**App not loading?**
- Make sure you've run `pnpm build` in the demo directory
- Check that `dist/app.js` exists
- Check browser console for import errors

**Compilation errors?**
- Make sure Erlang/OTP 26+ is installed: `erl -version`
- Install from https://www.erlang.org/downloads
- Check rebar3 is installed: `rebar3 version`

**WebSocket connection failed?**
- Check server logs for errors
- Verify port 8000 is not in use: `lsof -i :8000` or `netstat -an | grep 8000`
- Check firewall isn't blocking WebSocket connections

**Docker issues?**
- Make sure port 8000 is available
- Rebuild with `docker-compose build --no-cache`
- Check Docker has enough memory allocated

**Messages not broadcasting?**
- Check Erlang shell output for process errors
- Verify chat_room is running: `whereis(chat_room).`
- Check user count: `gen_server:call(chat_room, get_users).`

## Security Considerations

This is a **demo application** and should not be used in production without additional hardening:

- ⚠️ No authentication or authorization
- ⚠️ No input validation or sanitization
- ⚠️ No rate limiting (spam protection)
- ⚠️ No message persistence (data lost on restart)
- ⚠️ No HTTPS/TLS support
- ⚠️ No protection against malicious WebSocket clients

For production, add:
- User authentication (JWT, OAuth, etc.)
- Input sanitization to prevent XSS
- Rate limiting per user
- Message persistence (Mnesia, PostgreSQL, etc.)
- TLS/SSL encryption
- WebSocket message validation
- User permissions and roles
- Logging and monitoring
- Process limits to prevent resource exhaustion

## Why Erlang?

This demo demonstrates Switchback with a functional programming language optimized for concurrency:

- **Actor Model**: Pure message passing, no shared state
- **Fault Tolerance**: "Let it crash" with automatic recovery
- **Hot Code Reloading**: Update code without stopping the system
- **Distributed**: Built-in clustering across machines
- **Proven**: Powers WhatsApp, Discord, RabbitMQ, CouchDB
- **Scalability**: Handle millions of concurrent connections
- **Use Cases**: Chat, real-time systems, distributed systems, telecom

Perfect for building massively concurrent, fault-tolerant systems!

## Comparison with Other Demos

- **PHP Demo**: Traditional server-side rendering
- **Go Demo**: Concurrent worker pool with goroutines
- **Zig Demo**: Modern systems programming with manual memory management
- **C Demo**: Low-level HTTP server with optimistic updates
- **Rust Demo**: Memory safety with embedded database
- **Erlang Demo**: Actor model with message passing and fault tolerance

Each demonstrates different Switchback features with different language paradigms. **Erlang's actor model and fault tolerance are unique** - perfect for chat, real-time collaboration, and distributed systems.

## Learn More About Erlang/OTP

- [Learn You Some Erlang](https://learnyousomeerlang.com/) - Best Erlang tutorial
- [Erlang Official Docs](https://www.erlang.org/docs) - Reference documentation
- [OTP Design Principles](https://www.erlang.org/doc/design_principles/des_princ.html) - OTP patterns
- [Erlang in Anger](https://www.erlang-in-anger.com/) - Production debugging
- [The Erlang Rationale](http://www.erlang.se/publications/Ulf_Wiger.pdf) - Why Erlang exists

Erlang's concurrency model is one of the best for distributed systems - master it to build truly fault-tolerant applications!

## Next Steps

Try modifying the demoto:
1. Add private messaging between users
2. Create multiple chat rooms
3. Add message persistence with Mnesia
4. Implement typing indicators
5. Add emoji reactions to messages
6. Create a distributed cluster across multiple nodes
7. Add admin commands (kick, ban, mute)
8. Implement message search and history

Have fun exploring the Actor Model with Erlang and Switchback!
