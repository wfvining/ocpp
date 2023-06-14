-module(testing_handler).

-behaviour(ocpp_handler).

-export([init/1, boot_notification/2]).

init({error, Reason}) ->
    error(Reason);
init(_) ->
    {ok, nil}.

boot_notification(Notification, State) ->
    CustomData = ocpp_message:get(<<"customData">>, Notification),
    case ocpp_message:get(<<"testAction">>, CustomData) of
        <<"ACCEPT">>  ->
            {reply, make_response(<<"Accepted">>, CustomData), State};
        <<"REJECT">>  ->
            {reply, make_response(<<"Rejected">>, CustomData), State};
        <<"PENDING">> ->
            {reply, make_response(<<"Pending">>, CustomData), State};
        <<"ERROR">> ->
            {error, ocpp_message:get(<<"errorReason">>, CustomData), State}
    end.

make_response(Status, CustomData) ->
    #{'status' => Status,
      'interval' => ocpp_message:get(<<"interval">>, CustomData),
      'currentTime' => ocpp_message:get(<<"currentTime">>, CustomData)}.
