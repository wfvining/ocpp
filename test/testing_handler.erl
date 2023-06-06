-module(testing_handler).

-behaviour(ocpp_handler).

-export([init/1, boot_notification/1]).

init({error, Reason}) ->
    error(Reason);
init(_) ->
    {ok, nil}.

boot_notification(Notification) ->
    CustomData = ocpp_message:get(<<"customData">>, Notification),
    case jerk:get_value(CustomData, <<"testAction">>) of
        <<"ACCEPT">>  -> accepted;
        <<"REJECT">>  -> rejected;
        <<"PENDING">> -> pending
    end.
