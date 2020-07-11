-module(gem_recipe).
-include("gem.hrl").

-export([
    run/2
]).

-behaviour(ktn_recipe).

% ktn_recipe callbacks
-export([
    process_error/1,
    process_result/1,
    transitions/0
]).

% recipe steps
-export([
    read_url/1,
    parse_url/1,
    route/1,
    get_resource/1
]).

run(Data, Router) ->
    ktn_recipe:run(?MODULE, #{data => Data, router => Router}).

transitions() -> [
    read_url,
    parse_url,
    route,
    get_resource
].

process_error(#{error := Error, step := Step}) ->
    {StatusCode, Meta} = status_meta(Step, Error),
    make_response(StatusCode, Meta).

process_result(#{code := ?OK, body := Body}) ->
    make_response(?OK, <<"text/gemini;charset=utf-8">>, Body);
process_result(#{code := Code, meta := Meta}) ->
    make_response(Code, Meta).

% private

read_url(State = #{data := Data}) ->
    case binary:split(Data, <<?CRLF>>) of
        [Data] -> {error, State#{error => "unable to read URL", step => read_url}};
        [Url, _] -> {ok, State#{url => Url}}
    end.

parse_url(State = #{url := Url}) ->
    case uri_string:parse(Url) of
        Err = {error, _Reason, _Term} ->
            {error, State#{error => Err, step => parse_url}};
        Map -> {ok, State#{uri_map => Map}}
    end.

route(State = #{router := Router, uri_map := UriMap}) ->
    case Router:route(UriMap) of
        {ok, Handler} ->
            {ok, State#{module => Handler}};
        Err = {error, _} ->
            {error, State#{error => Err, step => route}}
    end.

get_resource(State = #{module := Module}) ->
    case Module:handle(State) of
        gone ->
            {error, State#{error => gone, step => get_resource}};
        not_found ->
            {error, State#{error => not_found, step => get_resource}};
        {bad_request, Message} ->
            {ok, State#{code => ?BAD_REQUEST, meta => Message}};
        {input, Prompt} ->
            {ok, State#{code => ?INPUT, meta => Prompt}};
        {sensitive_input, Prompt} ->
            {ok, State#{code => ?SENSITIVE_INPUT, meta => Prompt}};
        {redirect, Temporariness, Url} ->
            Code = case Temporariness of
                temporary -> ?REDIRECT_TEMPORARY;
                permanent -> ?REDIRECT_PERMANENT
            end,
            {ok, State#{code => Code, meta => Url}};
        {ok, Body} ->
            {ok, State#{code => ?OK, body => Body}}
    end.

make_response(Status, Meta) -> make_response(Status, Meta, <<"">>).

make_response(Status, Meta, Body) when
    is_integer(Status),
    is_binary(Meta)
  ->
    StatusBin = integer_to_binary(Status),
    iolist_to_binary([StatusBin, " ", Meta, <<?CRLF>>, Body]).

status_meta(read_url, _) ->
    {?BAD_REQUEST, <<"unable to read url">>};
status_meta(parse_url, _) ->
    {?BAD_REQUEST, <<"unable to parse url">>};
status_meta(_, gone) ->
    {?GONE, <<"gone">>};
status_meta(_, not_found) ->
    {?NOT_FOUND, <<"not found">>}.
