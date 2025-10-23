%%%-------------------------------------------------------------------
%%% @doc
%%% Page Handler - Handles Switchback page routing
%%% Returns JSON for Switchback requests, HTML for initial load
%%% @end
%%%-------------------------------------------------------------------
-module(page_handler).
-export([init/2]).

init(Req, State) ->
    Method = cowboy_req:method(Req),
    Path = cowboy_req:path(Req),

    % Check if this is a Switchback request (Cowboy normalizes headers to lowercase)
    IsSwitchback = cowboy_req:header(<<"x-switchback">>, Req) =/= undefined,

    Response = handle_route(Path, Method, IsSwitchback),

    Req2 = case IsSwitchback of
        true ->
            % Return JSON with HTML for Switchback SSR
            JsonBinary = jsx:encode(Response),
            cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json; charset=utf-8">>},
                JsonBinary,
                Req);
        false ->
            % Return full HTML page with initialPage (including SSR HTML)
            Html = render_html(Response),
            HtmlBinary = iolist_to_binary(Html),
            cowboy_req:reply(200,
                #{<<"content-type">> => <<"text/html; charset=utf-8">>},
                HtmlBinary,
                Req)
    end,

    {ok, Req2, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

handle_route(<<"/">>, <<"GET">>, _) ->
    {ok, RenderCount} = article_manager:increment_render_count(),
    {ok, Articles} = article_manager:get_all_articles(),
    {ok, Stats} = article_manager:get_stats(),

    Props = #{articles => Articles, stats => Stats, render_count => RenderCount},

    #{
        component => <<"Home">>,
        props => Props,
        url => <<"/">>,
        html => iolist_to_binary(html_renderer:render_page(<<"Home">>, Props, RenderCount))
    };

handle_route(<<"/articles/", IdBin/binary>>, <<"GET">>, _) ->
    {ok, RenderCount} = article_manager:increment_render_count(),
    try binary_to_integer(IdBin) of
        Id ->
            article_manager:increment_views(Id),

            case article_manager:get_article(Id) of
                {ok, Article} ->
                    Props = #{article => Article, render_count => RenderCount},
                    #{
                        component => <<"Article">>,
                        props => Props,
                        url => <<"/articles/", IdBin/binary>>,
                        html => iolist_to_binary(html_renderer:render_page(<<"Article">>, Props, RenderCount))
                    };
                {error, not_found} ->
                    Props = #{message => <<"Article not found">>, render_count => RenderCount},
                    #{
                        component => <<"Error">>,
                        props => Props,
                        url => <<"/">>,
                        html => iolist_to_binary(html_renderer:render_page(<<"Error">>, Props, RenderCount))
                    }
            end
    catch
        _:_ ->
            Props = #{message => <<"Invalid article ID">>, render_count => RenderCount},
            #{
                component => <<"Error">>,
                props => Props,
                url => <<"/">>,
                html => iolist_to_binary(html_renderer:render_page(<<"Error">>, Props, RenderCount))
            }
    end;

handle_route(<<"/about">>, <<"GET">>, _) ->
    {ok, RenderCount} = article_manager:increment_render_count(),
    Props = #{
        version => <<"2.0.0">>,
        backend => <<"Erlang/OTP">>,
        features => [
            <<"Server-side HTML rendering (SSR)">>,
            <<"HTML morphing with Switchback">>,
            <<"Actor model - gen_server manages articles">>,
            <<"View count tracking per article">>,
            <<"OTP supervision for fault tolerance">>,
            <<"Zero client-side JavaScript for rendering">>
        ],
        render_count => RenderCount
    },

    #{
        component => <<"About">>,
        props => Props,
        url => <<"/about">>,
        html => iolist_to_binary(html_renderer:render_page(<<"About">>, Props, RenderCount))
    };

handle_route(_, _, _) ->
    {ok, RenderCount} = article_manager:increment_render_count(),
    Props = #{message => <<"Page not found">>, render_count => RenderCount},

    #{
        component => <<"Error">>,
        props => Props,
        url => <<"/">>,
        html => iolist_to_binary(html_renderer:render_page(<<"Error">>, Props, RenderCount))
    }.

render_html(PageData) ->
    Json = jsx:encode(PageData),
    % Escape JSON for safe insertion in script tag (prevent </script> breakout)
    SafeJson = binary:replace(Json, <<"</">>, <<"<\\/">>, [global]),

    [
        <<"<!DOCTYPE html>\n">>,
        <<"<html lang=\"en\">\n">>,
        <<"<head>\n">>,
        <<"    <meta charset=\"UTF-8\">\n">>,
        <<"    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n">>,
        <<"    <title>Erlang Blog - Switchback SSR</title>\n">>,
        render_styles(),
        <<"    <script>window.initialPage = ">>, SafeJson, <<";</script>\n">>,
        <<"</head>\n">>,
        <<"<body>\n">>,
        <<"    <div data-swbk-app>\n">>,
        <<"        <div data-page=\"main\" style=\"padding: 2rem; text-align: center; background: #1a0b2e; color: #b794f6;\">\n">>,
        <<"            <p>Loading Erlang Blog...</p>\n">>,
        <<"        </div>\n">>,
        <<"    </div>\n">>,
        <<"    <script type=\"module\" src=\"/dist/app.js\"></script>\n">>,
        <<"</body>\n">>,
        <<"</html>">>
    ].

render_styles() ->
    [
        <<"<style>\n">>,
        <<"* { margin: 0; padding: 0; box-sizing: border-box; }\n">>,
        <<"body { font-family: 'Courier New', 'Consolas', monospace; background: #1a0b2e; color: #b794f6; line-height: 1.6; padding-bottom: 100vh; }\n">>,
        <<"nav { background: #2d1b4e; border-bottom: 2px solid #b794f6; padding: 1rem 2rem; position: sticky; top: 0; z-index: 100; box-shadow: 0 4px 12px rgba(183, 148, 246, 0.3); }\n">>,
        <<"nav .nav-content { max-width: 1200px; margin: 0 auto; display: flex; justify-content: space-between; align-items: center; }\n">>,
        <<"nav a { color: #b794f6; text-decoration: none; margin-right: 1.5rem; padding: 0.5rem 1rem; border: 1px solid transparent; transition: all 0.2s; font-weight: bold; text-shadow: 0 0 5px rgba(183, 148, 246, 0.5); }\n">>,
        <<"nav a:hover { border-color: #b794f6; background: rgba(183, 148, 246, 0.1); box-shadow: 0 0 10px rgba(183, 148, 246, 0.4); }\n">>,
        <<".nav-badge { background: #b794f6; color: #1a0b2e; padding: 0.4rem 0.8rem; border-radius: 4px; font-size: 0.85rem; font-weight: bold; }\n">>,
        <<"main { max-width: 1200px; margin: 2rem auto; padding: 0 2rem; animation: fadeIn 0.3s ease-out; }\n">>,
        <<"@keyframes fadeIn { from { opacity: 0; } to { opacity: 1; } }\n">>,
        <<".demo-hint { background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); border: 2px solid #b794f6; border-radius: 8px; padding: 1.25rem; margin-bottom: 2rem; color: white; line-height: 1.7; box-shadow: 0 4px 15px rgba(102, 126, 234, 0.4); }\n">>,
        <<".demo-hint strong { color: #ffd700; }\n">>,
        <<".hint-flash { background: rgba(255, 215, 0, 0.3); padding: 2px 6px; border-radius: 3px; font-weight: bold; color: #ffd700; border: 1px solid rgba(255, 215, 0, 0.5); }\n">>,
        <<".render-counter { margin-top: 0.75rem; padding-top: 0.75rem; border-top: 1px solid rgba(255, 255, 255, 0.2); font-size: 0.9rem; }\n">>,
        <<".counter-badge { background: #ffd700; color: #1a0b2e; padding: 3px 8px; border-radius: 4px; font-weight: bold; font-size: 1.1rem; margin-left: 0.5rem; }\n">>,
        <<"h1 { font-size: 1.8rem; margin-bottom: 0.75rem; color: #b794f6; text-shadow: 0 0 20px rgba(183, 148, 246, 0.6); }\n">>,
        <<"h2 { font-size: 1.4rem; color: #b794f6; margin: 1.5rem 0 0.75rem; text-shadow: 0 0 15px rgba(183, 148, 246, 0.5); }\n">>,
        <<".badge { background: #b794f6; color: #1a0b2e; padding: 0.2rem 0.6rem; border-radius: 4px; font-size: 0.85rem; margin-left: 0.5rem; display: inline-block; font-weight: bold; }\n">>,
        <<".terminal-box { background: #1a0b2e; border: 2px solid #b794f6; border-radius: 8px; padding: 1rem; margin: 1rem 0; font-family: 'Courier New', monospace; box-shadow: 0 0 15px rgba(183, 148, 246, 0.2); font-size: 0.9rem; }\n">>,
        <<".stats { display: grid; grid-template-columns: repeat(auto-fit, minmax(150px, 1fr)); gap: 0.75rem; margin: 1rem 0 1.5rem 0; }\n">>,
        <<".stat-card { background: #2d1b4e; border: 1px solid #b794f6; border-radius: 6px; padding: 0.75rem 1rem; text-align: center; box-shadow: 0 0 10px rgba(183, 148, 246, 0.15); }\n">>,
        <<".stat-card strong { font-size: 1.5rem; color: #b794f6; display: block; margin-bottom: 0.25rem; text-shadow: 0 0 10px rgba(183, 148, 246, 0.4); }\n">>,
        <<".stat-card div { font-size: 0.85rem; }\n">>,
        <<"button, .btn { background: #b794f6; color: #1a0b2e; border: none; padding: 0.75rem 1.5rem; border-radius: 6px; cursor: pointer; font-weight: bold; font-family: 'Courier New', monospace; text-decoration: none; display: inline-block; transition: all 0.2s; box-shadow: 0 0 10px rgba(183, 148, 246, 0.4); }\n">>,
        <<"button:hover, .btn:hover { transform: translateY(-2px); box-shadow: 0 4px 20px rgba(183, 148, 246, 0.6); }\n">>,
        <<"input, textarea { background: #2d1b4e; border: 2px solid #b794f6; color: #b794f6; padding: 0.75rem; border-radius: 6px; font-family: 'Courier New', monospace; font-size: 1rem; transition: all 0.2s; width: 100%; }\n">>,
        <<"input:focus, textarea:focus { outline: none; border-color: #d4a5f6; box-shadow: 0 0 15px rgba(183, 148, 246, 0.3); }\n">>,
        <<"input::placeholder, textarea::placeholder { color: rgba(183, 148, 246, 0.4); }\n">>,
        <<".info-box { background: #2d1b4e; border: 2px solid #b794f6; border-radius: 8px; padding: 1.5rem; margin: 1.5rem 0; box-shadow: 0 0 15px rgba(183, 148, 246, 0.2); }\n">>,
        <<"ul { margin-left: 2rem; margin-top: 1rem; }\n">>,
        <<"li { margin: 0.5rem 0; color: #d4a5f6; }\n">>,
        <<"code { background: #2d1b4e; padding: 0.2rem 0.5rem; border-radius: 3px; color: #ffd700; }\n">>,
        <<"a.back-link { color: #b794f6; text-decoration: none; display: inline-block; margin-top: 2rem; padding: 0.5rem 1rem; border: 1px solid #b794f6; border-radius: 4px; transition: all 0.2s; }\n">>,
        <<"a.back-link:hover { background: rgba(183, 148, 246, 0.1); box-shadow: 0 0 10px rgba(183, 148, 246, 0.3); }\n">>,
        <<".join-form { max-width: 500px; margin: 3rem auto; background: #2d1b4e; border: 2px solid #b794f6; border-radius: 8px; padding: 2rem; box-shadow: 0 0 20px rgba(183, 148, 246, 0.3); }\n">>,
        <<".join-form h2 { margin-top: 0; text-align: center; }\n">>,
        <<".form-group { margin: 1.5rem 0; }\n">>,
        <<".form-group label { display: block; margin-bottom: 0.5rem; color: #b794f6; }\n">>,
        <<".article-list { margin: 2rem 0; }\n">>,
        <<".article-row { background: #2d1b4e; border: 1px solid #b794f6; border-radius: 4px; padding: 1rem 1.5rem; margin-bottom: 0.75rem; transition: all 0.2s; }\n">>,
        <<".article-row:hover { background: #3d2b5e; box-shadow: 0 2px 8px rgba(183, 148, 246, 0.3); }\n">>,
        <<".article-row.viewed { background: #3d2410; border: 2px solid #FF9800; box-shadow: 0 0 12px rgba(255, 152, 0, 0.3); }\n">>,
        <<".article-row.viewed:hover { background: #4d3420; box-shadow: 0 0 16px rgba(255, 152, 0, 0.5); }\n">>,
        <<".article-title { font-size: 1.1rem; font-weight: bold; margin-bottom: 0.5rem; }\n">>,
        <<".article-title a { color: #d4a5f6; text-decoration: none; }\n">>,
        <<".article-title a:hover { color: #ffd700; text-decoration: underline; }\n">>,
        <<".article-row.viewed .article-title a { color: #FFB74D; }\n">>,
        <<".viewed-badge { background: #FF9800; color: white; padding: 2px 8px; border-radius: 3px; font-size: 0.75rem; font-weight: bold; margin-left: 0.5rem; }\n">>,
        <<".article-summary { color: rgba(183, 148, 246, 0.8); font-size: 0.95rem; margin-bottom: 0.5rem; line-height: 1.5; }\n">>,
        <<".article-meta { display: flex; gap: 0.5rem; font-size: 0.85rem; color: rgba(183, 148, 246, 0.6); flex-wrap: wrap; align-items: center; }\n">>,
        <<".article-meta .separator { margin: 0 0.25rem; }\n">>,
        <<".read-more { color: #b794f6; text-decoration: none; font-weight: bold; transition: color 0.2s; }\n">>,
        <<".read-more:hover { color: #ffd700; text-decoration: underline; }\n">>,
        <<".article-row.viewed .read-more { color: #FFB74D; }\n">>,
        <<".article-tags { display: inline-flex; gap: 0.5rem; }\n">>,
        <<".tag { background: rgba(183, 148, 246, 0.2); color: #d4a5f6; padding: 0.2rem 0.6rem; border-radius: 3px; font-size: 0.75rem; border: 1px solid rgba(183, 148, 246, 0.3); }\n">>,
        <<".article-detail { max-width: 800px; margin: 2rem auto; }\n">>,
        <<".article-header { border-bottom: 2px solid #b794f6; padding-bottom: 1.5rem; margin-bottom: 2rem; }\n">>,
        <<".article-content { line-height: 1.8; }\n">>,
        <<".article-content h2 { margin-top: 2rem; }\n">>,
        <<".article-content p { margin: 1rem 0; }\n">>,
        <<".article-content ul, .article-content ol { margin: 1rem 0 1rem 2rem; }\n">>,
        <<".article-content li { margin: 0.5rem 0; }\n">>,
        <<".article-footer { background: rgba(183, 148, 246, 0.1); border-left: 4px solid #b794f6; padding: 1rem; margin-top: 3rem; border-radius: 4px; }\n">>,
        <<"</style>\n">>
    ].
