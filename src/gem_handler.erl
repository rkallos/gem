-module(gem_handler).
-include("gem.hrl").

-callback handle(gem_req()) -> binary() | gone | not_found.
