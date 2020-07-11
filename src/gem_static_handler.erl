-module(gem_static_handler).
-include("gem.hrl").

-behaviour(gem_handler).

-export([
    handle/1
]).

-spec handle(gem_req()) -> binary() | gone | not_found.

handle(#{uri_map := #{path := <<$/, Path/binary>>}}) ->
    PageDir = case application:get_env(gem, static_page_dir) of
        undefined ->
            filename:join([code:priv_dir(gem), "pages"]);
        {ok, P} -> P
    end,
    PagePath = filename:join([PageDir, Path]),
    case filelib:is_file(PagePath) of
        true ->
            {ok, Bin} = file:read_file(PagePath),
            Bin;
        false -> not_found
    end.
