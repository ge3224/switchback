# Erlang Demo - Switchback SSR Integration

A blog article viewing application showcasing **Erlang/OTP's Actor Model** with **Server-Side Rendering (SSR)**. Experience stateful article management, view tracking, and instant first-paint rendering - combining the best of both worlds!

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
- **src/article_manager.erl** - Article manager using gen_server (Actor Model)
- **src/switchback_chat_sup.erl** - OTP supervisor for fault tolerance
- **src/html_renderer.erl** - Server-side HTML rendering module
- **src/page_handler.erl** - Returns HTML fragments for Switchback morphing
- **src/static_handler.erl** - Serves static files (JavaScript bundle)
- **app.ts** - Minimal client - just Switchback for navigation and article tracking
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

1. **OTP Supervisor** starts the article_manager gen_server
2. **User navigates to homepage** → article_manager returns list of all articles
3. **User clicks an article** → Switchback intercepts, fetches page data
4. **Server renders HTML** → article_manager increments view count, html_renderer generates HTML
5. **Switchback morphs DOM** → New article page appears without page reload
6. **View tracking** → Client-side code highlights last viewed article
7. **Navigation state** → All managed server-side with instant rendering

**The magic**: The gen_server maintains article state (view counts, render counts) while Switchback provides smooth SPA navigation with server-rendered HTML!

## Try It Out

Want to see Erlang's actor model in action without installing Erlang/OTP?

```bash
cd examples/demos/erlang
docker-compose up
```

Open http://localhost:8000

## Running Natively

To run this demo with a local Erlang installation, see the [Erlang downloads page](https://www.erlang.org/downloads).

## Try the Article Blog

1. Open http://localhost:8000 in your browser
2. View the list of 15 articles about Erlang and OTP
3. Click on any article to view its full content
4. Notice the instant navigation without page reloads
5. Watch the view counter increment for each article
6. Go back to the homepage and see your last viewed article highlighted
7. Check the server logs to see Erlang rendering HTML and tracking views!

The beauty: Server renders complete HTML, Switchback morphs it seamlessly, and gen_server manages all state!

## OTP Architecture

### Supervision Tree

```
switchback_chat_sup (supervisor)
    └── article_manager (gen_server)
            ├── Manages article data
            ├── Tracks view counts
            └── Tracks render statistics
```

If the article_manager crashes, the supervisor **automatically restarts it**. This is the "let it crash" philosophy - don't write defensive code, let supervisors handle failures!

### Process Structure

```
Browser
    ↓ HTTP Request (Switchback intercepts links)
    ↓
page_handler ──→ article_manager (gen_server)
    ↑                     ↓
    │         Returns article data + stats
    │                     ↓
    ├── html_renderer renders HTML
    │                     ↓
    └── Returns JSON with HTML to browser
                          ↓
            Switchback morphs HTML into DOM
```

When a user navigates:
1. Browser sends HTTP request with `X-Switchback` header
2. `page_handler` receives request and calls `article_manager`
3. `article_manager` (gen_server) updates view count via `handle_cast`
4. `article_manager` returns article data via `handle_call`
5. `html_renderer` generates HTML from article data
6. JSON response with HTML sent to browser
7. Switchback morphs HTML into existing DOM

### Code Walkthrough

**Fetching articles (synchronous call):**

```erlang
% In page_handler.erl - getting all articles
handle_route(<<"/">>, <<"GET">>, _) ->
    {ok, Articles} = article_manager:get_all_articles(),
    {ok, Stats} = article_manager:get_stats(),
    Props = #{articles => Articles, stats => Stats},
    ...
```

**Incrementing view count (async cast):**

```erlang
% In article_manager.erl - increment without blocking caller
handle_cast({increment_views, Id}, State) ->
    #state{articles = Articles, total_views = TotalViews} = State,
    case maps:find(Id, Articles) of
        {ok, Article} ->
            UpdatedArticle = Article#article{views = Article#article.views + 1},
            NewArticles = maps:put(Id, UpdatedArticle, Articles),
            {noreply, State#state{articles = NewArticles, total_views = TotalViews + 1}}
    end.
```

**Rendering HTML:**

```erlang
% In html_renderer.erl - server-side rendering
render_page(<<"Home">>, Props, RenderCount) ->
    Articles = maps:get(articles, Props),
    [<<"<div class='home-page'>">>,
     render_article_list(Articles),
     <<"</div>">>].
```

**Supervision (fault tolerance):**

```erlang
% In switchback_chat_sup.erl
init([]) ->
    ChildSpecs = [
        #{
            id => article_manager,
            start => {article_manager, start_link, []},
            restart => permanent,  % Always restart if it crashes!
            shutdown => 5000,
            type => worker
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.
```

## Key Features Demonstrated

- ✅ **Actor Model** - article_manager gen_server manages state
- ✅ **OTP gen_server** - Generic server behavior for article management
- ✅ **OTP Supervision** - Automatic restart on crashes
- ✅ **Server-Side Rendering** - Complete HTML generated by Erlang
- ✅ **HTML Morphing** - Switchback efficiently updates DOM
- ✅ **State Management** - View counts and stats tracked server-side
- ✅ **Instant First Paint** - SEO-friendly, works without JavaScript
- ✅ **Smooth Navigation** - SPA experience with server control
- ✅ **View Tracking** - Client-side enhancement for last viewed article
- ✅ **Beautiful UI** - Purple theme showcasing Erlang!

## File Structure

```
erlang/
├── src/
│   ├── switchback_chat.erl        # Main application entry point
│   ├── switchback_chat_app.erl    # OTP application callback
│   ├── switchback_chat_sup.erl    # OTP supervisor
│   ├── article_manager.erl        # Article manager gen_server
│   ├── html_renderer.erl          # Server-side HTML rendering
│   ├── page_handler.erl           # Switchback page routing
│   ├── static_handler.erl         # Static file serving
│   ├── stats_handler.erl          # Stats API endpoint
│   └── users_handler.erl          # Users API endpoint
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

### Add Article Comments

In `article_manager.erl`:

```erlang
-record(state, {
    articles = #{},
    comments = #{},  % #{ArticleId => [Comment]}
    ...
}).

handle_cast({add_comment, ArticleId, Comment}, State) ->
    #state{comments = Comments} = State,
    ArticleComments = maps:get(ArticleId, Comments, []),
    NewComments = maps:put(ArticleId, [Comment | ArticleComments], Comments),
    {noreply, State#state{comments = NewComments}}.
```

### Add Article Search

Implement full-text search across articles:

```erlang
handle_call({search_articles, Query}, _From, State) ->
    #state{articles = Articles} = State,
    Results = maps:filter(fun(_Id, Article) ->
        TitleMatch = string:find(Article#article.title, Query) =/= nomatch,
        ContentMatch = string:find(Article#article.content, Query) =/= nomatch,
        TitleMatch orelse ContentMatch
    end, Articles),
    {reply, {ok, maps:values(Results)}, State}.
```

### Add Persistence

Use Mnesia (Erlang's distributed database):

```erlang
% In article_manager.erl
-record(article_record, {id, title, author, date, summary, content, views, tags}).

init([]) ->
    mnesia:create_table(article_record,
        [{attributes, record_info(fields, article_record)},
         {disc_copies, [node()]}]),

    % Load articles from Mnesia or initialize with defaults
    Articles = load_articles_from_mnesia(),
    {ok, #state{articles = Articles}}.

save_article(Article) ->
    mnesia:dirty_write(#article_record{
        id = Article#article.id,
        title = Article#article.title,
        views = Article#article.views,
        ...
    }).
```

### Add Distributed Nodes

Run the blog on multiple servers with shared article state:

```bash
# Node 1
erl -name blog1@server1.com -setcookie mysecret

# Node 2
erl -name blog2@server2.com -setcookie mysecret

# Connect nodes
(blog2@server2.com)> net_adm:ping('blog1@server1.com').
pong

# Now article_manager state can be synchronized across machines!
# Use Mnesia replication for distributed article storage
```

### Add Article Ratings

Track user ratings for articles:

```erlang
-record(state, {
    articles = #{},
    ratings = #{}  % #{ArticleId => {TotalRating, Count}}
}).

handle_cast({rate_article, ArticleId, Rating}, State) ->
    #state{ratings = Ratings} = State,
    {Total, Count} = maps:get(ArticleId, Ratings, {0, 0}),
    NewRatings = maps:put(ArticleId, {Total + Rating, Count + 1}, Ratings),
    AvgRating = (Total + Rating) / (Count + 1),
    io:format("Article ~p rated: ~.2f/5.0~n", [ArticleId, AvgRating]),
    {noreply, State#state{ratings = NewRatings}}.
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
3. Find the article_manager process:

```erlang
whereis(article_manager).
% Returns: <0.123.0>
```

4. Kill it manually:

```erlang
exit(whereis(article_manager), kill).
```

5. **Watch it restart automatically!**

```erlang
whereis(article_manager).
% Returns: <0.456.0> (different PID!)
```

The supervisor detected the crash and restarted the process. Article data will be reinitialized, and the application continues working!

## Troubleshooting

**App not loading?**
- Make sure you've run `pnpm build` in the demo directory
- Check that `dist/app.js` exists
- Check browser console for import errors

**Compilation errors?**
- Make sure Erlang/OTP 26+ is installed: `erl -version`
- Install from https://www.erlang.org/downloads
- Check rebar3 is installed: `rebar3 version`

**HTTP requests failing?**
- Check server logs for errors
- Verify port 8000 is not in use: `lsof -i :8000` or `netstat -an | grep 8000`
- Ensure Cowboy HTTP server started properly

**Docker issues?**
- Make sure port 8000 is available
- Rebuild with `docker-compose build --no-cache`
- Check Docker has enough memory allocated

**Articles not loading?**
- Check Erlang shell output for process errors
- Verify article_manager is running: `whereis(article_manager).`
- Check article count: `gen_server:call(article_manager, get_stats).`

## Security Considerations

This is a **demo application** and should not be used in production without additional hardening:

- ⚠️ No authentication or authorization
- ⚠️ No input validation or sanitization
- ⚠️ No rate limiting on view counts
- ⚠️ No article persistence (data lost on restart)
- ⚠️ No HTTPS/TLS support
- ⚠️ No protection against malicious clients

For production, add:
- User authentication (JWT, OAuth, etc.)
- Input sanitization to prevent XSS
- Rate limiting on API endpoints
- Article persistence (Mnesia, PostgreSQL, etc.)
- TLS/SSL encryption
- Content Security Policy (CSP)
- User permissions for article management
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
- **Erlang Demo**: Actor model with server-side HTML rendering and fault tolerance

Each demonstrates different Switchback features with different language paradigms. **Erlang's SSR with HTML morphing is unique** - perfect for SEO-friendly, fast-loading web applications with server-controlled state management.

## Learn More About Erlang/OTP

- [Learn You Some Erlang](https://learnyousomeerlang.com/) - Best Erlang tutorial
- [Erlang Official Docs](https://www.erlang.org/docs) - Reference documentation
- [OTP Design Principles](https://www.erlang.org/doc/design_principles/des_princ.html) - OTP patterns
- [Erlang in Anger](https://www.erlang-in-anger.com/) - Production debugging
- [The Erlang Rationale](http://www.erlang.se/publications/Ulf_Wiger.pdf) - Why Erlang exists

Erlang's concurrency model is one of the best for distributed systems - master it to build truly fault-tolerant applications!

## Next Steps

Try modifying the demo to:
1. Add article comments with real-time updates
2. Implement article search functionality
3. Add article persistence with Mnesia
4. Create article categories and filtering
5. Add user ratings for articles
6. Create a distributed cluster across multiple nodes
7. Add article authoring and editing
8. Implement article recommendations based on view history

Have fun exploring Server-Side Rendering with Erlang and Switchback!
