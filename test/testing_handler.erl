-module(testing_handler).

-behaviour(ocpp_handler).

-export([init/1, boot_notification/2, handle_call_response/2]).

init({error, Reason}) ->
    error(Reason);
init(ForwardPid) ->
    {ok, ForwardPid}.

boot_notification(Notification, State) ->
    CustomData = ocpp_message:get(<<"customData">>, Notification),
    case catch ocpp_message:get(<<"delay">>, CustomData) of
        {'EXIT', _} -> ok;
        Delay when is_number(Delay) ->
            timer:sleep(Delay)
    end,
    case ocpp_message:get(<<"testAction">>, CustomData) of
        <<"ACCEPT">>  ->
            {reply, make_response(<<"Accepted">>, CustomData), State};
        <<"REJECT">>  ->
            {reply, make_response(<<"Rejected">>, CustomData), State};
        <<"PENDING">> ->
            {reply, make_response(<<"Pending">>, CustomData), State};
        <<"ERROR">> ->
            {error, ocpp_message:get(<<"errorReason">>, CustomData), State};
        <<"CRASH">> ->
            error(ocpp_message:get(<<"errorReason">>, CustomData));
        <<"EXIT">> ->
            exit(ocpp_message:get(<<"errorReason">>, CustomData))
    end.

handle_call_response(Msg, ForwardPid) ->
    ForwardPid ! Msg,
    {ok, ForwardPid}.

make_response(Status, CustomData) ->
    #{'status' => Status,
      'interval' => ocpp_message:get(<<"interval">>, CustomData),
      'currentTime' => ocpp_message:get(<<"currentTime">>, CustomData)}.
