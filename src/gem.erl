-module(gem).

-export([
    start_listener/3,
    stop_listener/1
]).

start_listener(Name, SslOpts, GemOpts) ->
    {ok, _} = ranch:start_listener(Name, ranch_ssl, SslOpts,
        gem_protocol, GemOpts).

stop_listener(Name) ->
    ok = ranch:stop_listener(Name).
