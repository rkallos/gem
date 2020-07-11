-module(gem_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ok = ssl:start(),
    Priv = code:priv_dir(gem),
    {ok, Router} = application:get_env(router_module),
    SslOpts = [
        {certfile, filename:join([Priv, "server.cert"])},
        {keyfile, filename:join([Priv, "server.key"])},
        {port, 1965},
        {protocol, tls},
        {versions, ['tlsv1.3']}
    ],
    {ok, _} = ranch:start_listener(gem, ranch_ssl, SslOpts, gem_protocol, []),
    %persistent_term:put(gem_router, Router),
    gem_sup:start_link().

stop(_State) ->
    %persistent_term:erase(gem_router),
    ranch:stop_listener(gem),
    ok.
