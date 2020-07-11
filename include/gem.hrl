-define(CRLF, "\r\n").

-type gem_req() :: #{
    module := atom(),
    uri_map := uri_string:uri_map(),
    url := binary()
}.

-define(INPUT, 10).
-define(SENSITIVE_INPUT, 11).
-define(OK, 20).
-define(REDIRECT_TEMPORARY, 30).
-define(REDIRECT_PERMANENT, 31).
-define(NOT_FOUND, 51).
-define(GONE, 52).
-define(BAD_REQUEST, 59).
