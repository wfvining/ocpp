-module(ocpp_message).

-export([get/2, fields/1, request/2, response/2]).

-export_type([kind/0, action/0, message/0, payload/0]).

-type fieldname() :: binary().
-type kind() :: request | response.
-type action() :: binary().
%% -type id_string() :: [$a..$z | $A..$Z | $0..$9 | $* | $- |
%%                       $_ | $= | $: | $+ | $@ | $.].
-type id_string() :: binary().
-type primitive() :: binary()
                   | id_string()
                   | integer()
                   | float()
                   | calendar:datetime()
                   | boolean().
-opaque payload() :: #{fieldname() => payload() | primitive()}.
-type message() :: {kind(), action(), payload()}.

-spec get(Field :: fieldname(), Payload :: payload()) -> any().
get(Field, Payload) ->
    maps:get(Field, Payload).

-spec fields(Payload :: payload()) -> [fieldname()].
fields(Payload) ->
    maps:keys(Payload).

-spec request(Action :: action(), Payload :: map()) -> message().
request(Action, Payload) ->
    case ocpp_schema:validate(<<Action/binary, "Request">>, Payload) of
        ok ->
            {request, Action, Payload};
        {error, Reason} ->
            error(Reason)
    end.

-spec response(Action :: action(), Payload :: map()) -> message().
response(Action, Payload) ->
    case ocpp_schema:validate(<<Action/binary, "Response">>, Payload) of
        {ok, _Payload} ->
            {response, Action, Payload};
        {error, Reason} ->
            error(Reason)
    end.
