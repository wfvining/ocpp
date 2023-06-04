-module(do_nothing_handler).

-behaviour(ocpp_handler).

-export([init/1]).

init(_) ->
    {ok, nil}.
