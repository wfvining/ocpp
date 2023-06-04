-module(do_nothing_handler).

-behaviour(ocpp_handler).

-export([init/1]).

init({error, Reason}) ->
    error(Reason);
init(_) ->
    {ok, nil}.
