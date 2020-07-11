-module(gem_protocol).
-include("gem.hrl").

-export([init/3]).

-behaviour(ranch_protocol).
-export([start_link/3]).

start_link(Ref, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Transport, Opts]),
    {ok, Pid}.

% private

activate(Transport, Socket) ->
    Transport:setopts(Socket, [{active, once}]).

init(Ref, Transport, Opts) ->
    {router, Router} = lists:keyfind(router, 1, Opts),
    {ok, Socket} = ranch:handshake(Ref),
    ok = activate(Transport, Socket),
    {Ok, Closed, Error, _Passive} = Transport:messages(),
    receive
        {Ok, Socket, Data} ->
            Resp = gem_recipe:run(Data, Router),
            Transport:send(Socket, Resp);
        {Closed, Socket} ->
            % TODO: log
            ok;
        {Error, Socket, _Reason} ->
            % TODO: log
            ok
    end,
    Transport:close(Socket).
