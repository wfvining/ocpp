-module(basic_handler).

-behaviour(ocpp_handler).

-export([init/1, handle_ocpp/3, handle_info/2]).

init({}) ->
    {ok, nil}.

handle_ocpp(_, Message, State) ->
    {error, ocpp_error:new('NotSupported', ocpp_message:id(Message)), State}.

handle_info(_, State) ->
    {ok, State}.
