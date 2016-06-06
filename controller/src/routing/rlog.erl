-module(rlog).

-export([log/2, debug/2]).

-define(DO_DEBUG, false).

log(Str, Args) ->
    error_logger:info_msg(Str, Args).

debug(Str, Args) ->
    if ?DO_DEBUG  ->
        log(Str, Args);
       true ->
	ok
    end.