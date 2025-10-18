%%%-------------------------------------------------------------------
%%% @doc
%%% Static File Handler - Serves static files with correct MIME types
%%% @end
%%%-------------------------------------------------------------------
-module(static_handler).
-export([init/2]).

init(Req, State) ->
    Path = cowboy_req:path(Req),

    % Remove /dist/ prefix and get the file path
    FilePath = case Path of
        <<"/dist/", Rest/binary>> ->
            binary_to_list(<<"dist/", Rest/binary>>);
        _ ->
            "dist/app.js"
    end,

    case file:read_file(FilePath) of
        {ok, Content} ->
            ContentType = get_content_type(FilePath),
            Req2 = cowboy_req:reply(200,
                #{<<"content-type">> => ContentType},
                Content,
                Req),
            {ok, Req2, State};
        {error, _} ->
            Req2 = cowboy_req:reply(404,
                #{<<"content-type">> => <<"text/plain">>},
                <<"File not found">>,
                Req),
            {ok, Req2, State}
    end.

get_content_type(FilePath) ->
    case filename:extension(FilePath) of
        ".js" -> <<"application/javascript">>;
        ".mjs" -> <<"application/javascript">>;
        ".css" -> <<"text/css">>;
        ".html" -> <<"text/html">>;
        ".json" -> <<"application/json">>;
        ".map" -> <<"application/json">>;
        ".png" -> <<"image/png">>;
        ".jpg" -> <<"image/jpeg">>;
        ".jpeg" -> <<"image/jpeg">>;
        ".gif" -> <<"image/gif">>;
        ".svg" -> <<"image/svg+xml">>;
        ".ico" -> <<"image/x-icon">>;
        _ -> <<"application/octet-stream">>
    end.
