%% -*- coding: utf-8 -*-
%%%-------------------------------------------------------------------
%%% @doc
%%% Article Manager - Manages blog articles using gen_server
%%% Demonstrates Erlang's state management with view counts
%%% @end
%%%-------------------------------------------------------------------
-module(article_manager).
-behaviour(gen_server).

%% API
-export([start_link/0, get_all_articles/0, get_article/1, increment_views/1, get_stats/0, increment_render_count/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(article, {
    id,
    title,
    author,
    date,
    summary,
    content,
    views = 0,
    tags = []
}).

-record(state, {
    articles = #{},
    total_views = 0,
    render_count = 0,
    last_viewed_article_id = undefined
}).

%%====================================================================
%% API Functions
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_all_articles() ->
    gen_server:call(?MODULE, get_all_articles).

get_article(Id) ->
    gen_server:call(?MODULE, {get_article, Id}).

increment_views(Id) ->
    gen_server:cast(?MODULE, {increment_views, Id}).

get_stats() ->
    gen_server:call(?MODULE, get_stats).

increment_render_count() ->
    gen_server:call(?MODULE, increment_render_count).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init([]) ->
    io:format("🟣 Article manager initialized~n"),

    Articles = #{
        1 => #article{
            id = 1,
            title = <<"Introduction to Erlang's Actor Model">>,
            author = <<"Joe Armstrong">>,
            date = <<"2024-01-15">>,
            summary = <<"Learn how Erlang's lightweight processes enable massive concurrency through the Actor Model.">>,
            content = <<
                "<h2>What is the Actor Model?</h2>"
                "<p>The Actor Model is a concurrency paradigm where independent processes communicate solely by sending messages. "
                "Unlike traditional threading models with shared memory, actors are completely isolated.</p>"
                "<h2>Erlang's Implementation</h2>"
                "<p>Erlang processes are incredibly lightweight - you can spawn millions on a single machine. Each process has:</p>"
                "<ul>"
                "<li>Its own isolated memory space</li>"
                "<li>A private mailbox for receiving messages</li>"
                "<li>The ability to create new processes</li>"
                "<li>Built-in error handling and supervision</li>"
                "</ul>"
                "<h2>Why It Matters</h2>"
                "<p>This architecture powers systems like WhatsApp (3 million concurrent connections per server) and Discord. "
                "The 'let it crash' philosophy combined with supervision trees creates incredibly fault-tolerant systems.</p>"
            >>,
            tags = [<<"erlang">>, <<"concurrency">>, <<"actor-model">>]
        },

        2 => #article{
            id = 2,
            title = <<"Building Real-Time Systems with OTP">>,
            author = <<"Francesco Cesarini">>,
            date = <<"2024-02-01">>,
            summary = <<"Discover how OTP behaviors provide battle-tested patterns for building reliable distributed systems.">>,
            content = <<
                "<h2>What is OTP?</h2>"
                "<p>OTP (Open Telecom Platform) is a collection of libraries, design principles, and behaviors that make "
                "building robust systems easier. It's been battle-tested for decades in telecom infrastructure.</p>"
                "<h2>Core Behaviors</h2>"
                "<p><strong>gen_server</strong> - Generic server for request-response patterns</p>"
                "<p><strong>supervisor</strong> - Monitors and restarts child processes automatically</p>"
                "<p><strong>gen_statem</strong> - State machine implementation for complex workflows</p>"
                "<h2>Real-World Success</h2>"
                "<p>Ericsson's AXD301 ATM switch achieved 99.9999999% (nine nines) reliability using OTP. "
                "That's less than 32 milliseconds of downtime per year!</p>"
            >>,
            tags = [<<"erlang">>, <<"otp">>, <<"distributed-systems">>]
        },

        3 => #article{
            id = 3,
            title = <<"Server-Side Rendering with Switchback">>,
            author = <<"Switchback Team">>,
            date = <<"2024-03-10">>,
            summary = <<"Explore how Switchback enables fast, SEO-friendly server-side rendering with any backend stack.">>,
            content = <<
                "<h2>The SSR Problem</h2>"
                "<p>Traditional SPAs render everything client-side, causing:</p>"
                "<ul>"
                "<li>Slow first paint - users see blank pages</li>"
                "<li>Poor SEO - search engines see no content</li>"
                "<li>Large JavaScript bundles - slow to download and parse</li>"
                "</ul>"
                "<h2>Switchback's Solution</h2>"
                "<p>Server renders complete HTML, Switchback morphs it into the DOM:</p>"
                "<ol>"
                "<li>Server generates full HTML (instant first paint)</li>"
                "<li>Browser displays content immediately (SEO-friendly)</li>"
                "<li>Switchback loads and enhances navigation (SPA experience)</li>"
                "<li>Subsequent navigations morph HTML efficiently (no flicker)</li>"
                "</ol>"
                "<h2>Best of Both Worlds</h2>"
                "<p>You get progressive enhancement: fast initial load, smooth navigation, and it works without JavaScript!</p>"
            >>,
            tags = [<<"switchback">>, <<"ssr">>, <<"web-development">>]
        },

        4 => #article{
            id = 4,
            title = <<"Fault Tolerance: The 'Let It Crash' Philosophy">>,
            author = <<"Robert Virding">>,
            date = <<"2024-03-20">>,
            summary = <<"Learn why Erlang's approach to errors is fundamentally different from traditional exception handling.">>,
            content = <<
                "<h2>Traditional Error Handling</h2>"
                "<p>Most languages encourage defensive programming:</p>"
                "<ul>"
                "<li>Try/catch blocks everywhere</li>"
                "<li>Checking for null/undefined constantly</li>"
                "<li>Complex error recovery logic</li>"
                "</ul>"
                "<h2>Erlang's Approach</h2>"
                "<p>Instead of preventing crashes, embrace them:</p>"
                "<ol>"
                "<li>Write code for the happy path</li>"
                "<li>Let processes crash on errors</li>"
                "<li>Supervisors automatically restart failed processes</li>"
                "<li>System continues operating despite failures</li>"
                "</ol>"
                "<h2>Why It Works</h2>"
                "<p>Supervision trees isolate failures. A crashed process doesn't take down the whole system. "
                "The supervisor can restart it with clean state, often fixing transient errors automatically.</p>"
            >>,
            tags = [<<"erlang">>, <<"fault-tolerance">>, <<"supervision">>]
        },

        5 => #article{
            id = 5,
            title = <<"Message Passing vs Shared Memory">>,
            author = <<"Erlang Community">>,
            date = <<"2024-04-05">>,
            summary = <<"Compare Erlang's message-passing concurrency with traditional shared-memory threading models.">>,
            content = <<
                "<h2>The Shared Memory Problem</h2>"
                "<p>Traditional threads share memory, leading to:</p>"
                "<ul>"
                "<li>Race conditions and deadlocks</li>"
                "<li>Complex locking mechanisms</li>"
                "<li>Hard-to-reproduce bugs</li>"
                "<li>Performance bottlenecks from lock contention</li>"
                "</ul>"
                "<h2>Message Passing Benefits</h2>"
                "<p>Erlang processes communicate by sending messages:</p>"
                "<ul>"
                "<li>No shared state = no race conditions</li>"
                "<li>No locks needed = better performance at scale</li>"
                "<li>Easy to reason about = fewer bugs</li>"
                "<li>Natural fit for distributed systems</li>"
                "</ul>"
                "<h2>Performance Considerations</h2>"
                "<p>While message passing has overhead, Erlang optimizes it heavily. For large-scale concurrent systems, "
                "the benefits of isolation outweigh the messaging cost.</p>"
            >>,
            tags = [<<"erlang">>, <<"concurrency">>, <<"performance">>]
        },

        6 => #article{
            id = 6,
            title = <<"Hot Code Swapping in Production">>,
            author = <<"Martin Logan">>,
            date = <<"2024-04-15">>,
            summary = <<"Learn how Erlang enables updating running systems without downtime through hot code reloading.">>,
            content = <<"<h2>Zero Downtime Deployments</h2><p>Erlang allows you to update code in a running system without stopping it. This is critical for systems that must stay online 24/7.</p>">>,
            tags = [<<"erlang">>, <<"deployment">>, <<"hot-reload">>]
        },

        7 => #article{
            id = 7,
            title = <<"Pattern Matching for Flow Control">>,
            author = <<"Erlang Team">>,
            date = <<"2024-04-20">>,
            summary = <<"Discover how Erlang's pattern matching eliminates complex conditional logic and makes code more readable.">>,
            content = <<"<h2>Beyond Simple Conditionals</h2><p>Pattern matching in Erlang function clauses provides elegant flow control without nested if/else statements.</p>">>,
            tags = [<<"erlang">>, <<"syntax">>, <<"patterns">>]
        },

        8 => #article{
            id = 8,
            title = <<"ETS: Erlang's Built-in Database">>,
            author = <<"Database Team">>,
            date = <<"2024-04-25">>,
            summary = <<"Explore ETS tables for high-performance in-memory storage with millions of operations per second.">>,
            content = <<"<h2>Fast In-Memory Storage</h2><p>ETS provides concurrent access to shared data without locks, making it perfect for caching and session storage.</p>">>,
            tags = [<<"erlang">>, <<"database">>, <<"ets">>]
        },

        9 => #article{
            id = 9,
            title = <<"Distributed Erlang Clusters">>,
            author = <<"Cluster Team">>,
            date = <<"2024-05-01">>,
            summary = <<"Build distributed systems that span multiple machines with transparent process communication.">>,
            content = <<"<h2>Location Transparency</h2><p>Send messages to processes on remote nodes as easily as local ones. Erlang handles all the networking.</p>">>,
            tags = [<<"erlang">>, <<"distributed">>, <<"clustering">>]
        },

        10 => #article{
            id = 10,
            title = <<"Understanding Erlang Binaries">>,
            author = <<"Binary Expert">>,
            date = <<"2024-05-05">>,
            summary = <<"Master efficient binary data handling for parsing protocols and working with raw data streams.">>,
            content = <<"<h2>Efficient Binary Operations</h2><p>Erlang's binary syntax makes parsing network protocols and file formats incredibly efficient.</p>">>,
            tags = [<<"erlang">>, <<"binaries">>, <<"performance">>]
        },

        11 => #article{
            id = 11,
            title = <<"GenStatem for Complex State Machines">>,
            author = <<"State Machine Team">>,
            date = <<"2024-05-10">>,
            summary = <<"Implement robust state machines using OTP's gen_statem behavior for complex workflows.">>,
            content = <<"<h2>State Machine Patterns</h2><p>GenStatem provides a clean way to model systems with multiple states and transitions between them.</p>">>,
            tags = [<<"erlang">>, <<"otp">>, <<"state-machines">>]
        },

        12 => #article{
            id = 12,
            title = <<"Erlang for Embedded Systems">>,
            author = <<"Embedded Team">>,
            date = <<"2024-05-15">>,
            summary = <<"Run Erlang on IoT devices and embedded hardware with minimal resource requirements.">>,
            content = <<"<h2>Small Footprint Runtime</h2><p>Erlang's VM can run on resource-constrained devices while maintaining fault tolerance.</p>">>,
            tags = [<<"erlang">>, <<"embedded">>, <<"iot">>]
        },

        13 => #article{
            id = 13,
            title = <<"Debugging Erlang Applications">>,
            author = <<"Debug Team">>,
            date = <<"2024-05-20">>,
            summary = <<"Master debugging tools like dbg, recon, and observer to troubleshoot production systems.">>,
            content = <<"<h2>Production Debugging</h2><p>Erlang provides powerful introspection tools for debugging running systems without stopping them.</p>">>,
            tags = [<<"erlang">>, <<"debugging">>, <<"tools">>]
        },

        14 => #article{
            id = 14,
            title = <<"Erlang Type Specifications">>,
            author = <<"Type Team">>,
            date = <<"2024-05-25">>,
            summary = <<"Add type specs and use Dialyzer for static analysis to catch bugs before runtime.">>,
            content = <<"<h2>Static Analysis</h2><p>Type specs help document code and enable Dialyzer to find type errors at compile time.</p>">>,
            tags = [<<"erlang">>, <<"types">>, <<"dialyzer">>]
        },

        15 => #article{
            id = 15,
            title = <<"Property-Based Testing with PropEr">>,
            author = <<"Testing Team">>,
            date = <<"2024-05-30">>,
            summary = <<"Write fewer tests that find more bugs using property-based testing frameworks.">>,
            content = <<"<h2>Generative Testing</h2><p>PropEr generates hundreds of test cases from properties you define, finding edge cases you'd never think of.</p>">>,
            tags = [<<"erlang">>, <<"testing">>, <<"proper">>]
        }
    },

    {ok, #state{articles = Articles}}.

handle_call(get_all_articles, _From, State) ->
    #state{articles = Articles} = State,

    % Convert articles to maps for easier serialization
    ArticleList = lists:sort(
        fun(A, B) -> maps:get(id, A) >= maps:get(id, B) end,
        [article_to_map(Article) || Article <- maps:values(Articles)]
    ),

    {reply, {ok, ArticleList}, State};

handle_call({get_article, Id}, _From, State) ->
    #state{articles = Articles} = State,

    case maps:find(Id, Articles) of
        {ok, Article} ->
            {reply, {ok, article_to_map(Article)}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call(get_stats, _From, State) ->
    #state{articles = Articles, total_views = TotalViews, render_count = RenderCount, last_viewed_article_id = LastViewedId} = State,

    Stats = #{
        total_articles => maps:size(Articles),
        total_views => TotalViews,
        most_viewed => get_most_viewed(Articles),
        render_count => RenderCount,
        last_viewed_article_id => LastViewedId
    },

    {reply, {ok, Stats}, State};

handle_call(increment_render_count, _From, State) ->
    #state{render_count = RenderCount} = State,
    NewCount = RenderCount + 1,
    NewState = State#state{render_count = NewCount},
    {reply, {ok, NewCount}, NewState};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({increment_views, Id}, State) ->
    #state{articles = Articles, total_views = TotalViews} = State,

    case maps:find(Id, Articles) of
        {ok, Article} ->
            UpdatedArticle = Article#article{views = Article#article.views + 1},
            NewArticles = maps:put(Id, UpdatedArticle, Articles),
            NewState = State#state{
                articles = NewArticles,
                total_views = TotalViews + 1,
                last_viewed_article_id = Id
            },
            io:format("🟣 Article ~p views: ~p (marked as last viewed)~n", [Id, UpdatedArticle#article.views]),
            {noreply, NewState};
        error ->
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

article_to_map(Article) ->
    #{
        id => Article#article.id,
        title => Article#article.title,
        author => Article#article.author,
        date => Article#article.date,
        summary => Article#article.summary,
        content => Article#article.content,
        views => Article#article.views,
        tags => Article#article.tags
    }.

get_most_viewed(Articles) ->
    case maps:size(Articles) of
        0 -> none;
        _ ->
            MostViewed = lists:foldl(
                fun(Article, Acc) ->
                    case Acc of
                        none -> Article;
                        _ when Article#article.views > Acc#article.views -> Article;
                        _ -> Acc
                    end
                end,
                none,
                maps:values(Articles)
            ),
            case MostViewed of
                none -> none;
                _ -> article_to_map(MostViewed)
            end
    end.
