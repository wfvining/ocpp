-module(testing_handler).

-behaviour(ocpp_handler).

-export([init/1, boot_notification/2]).

init({error, Reason}) ->
    error(Reason);
init(_) ->
    {ok, nil}.

boot_notification(Notification, State) ->
    CustomData = ocpp_message:get(<<"customData">>, Notification),
    Response = case ocpp_message:get(<<"testAction">>, CustomData) of
                   <<"ACCEPT">>  ->
                       make_response(<<"Accepted">>, CustomData);
                   <<"REJECT">>  ->
                       make_response(<<"Rejected">>, CustomData);
                   <<"PENDING">> ->
                       make_response(<<"Pending">>, CustomData)
               end,
    {reply, Response, State}.

make_response(Status, CustomData) ->
    #{'status' => Status,
      'interval' => ocpp_message:get(<<"interval">>, CustomData),
      'currentTime' => ocpp_message:get(<<"currentTime">>, CustomData)}.
