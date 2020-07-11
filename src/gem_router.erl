-module(gem_router).

-type path() :: binary().
-type handler_module() :: atom().

-callback route(path()) -> {ok, handler_module()} | {error, term()}.
