-module(ocpp_message).

-export([new_request/2, new_request/3, new_response/3,
         properties/1, get/2, get/3, id/1,
         type/1, request_type/1, response_type/1]).

-type message_body() :: #{binary() | string() => jerk:primterm() | message_body()}
                      | [{binary() | string(), jerk:primterm() | message_body()}].

-export_type([message/0, messageid/0]).

-type messagetype() :: 'Authorize'
                     | 'BootNotification'
                     | 'CancelReservation'
                     | 'CertificateSigned'
                     | 'ChangeAvailability'
                     | 'ClearCache'
                     | 'ClearChargingProfile'
                     | 'ClearDisplayMessage'
                     | 'ClearedChargingLimit'
                     | 'ClearVariableMonitoring'
                     | 'CostUpdated'
                     | 'CustomerInformation'
                     | 'DataTransfer'
                     | 'DeleteCertificate'
                     | 'FirmwareStatusNotification'
                     | 'Get15118EVCertificate'
                     | 'GetBaseReport'
                     | 'GetCertificateStatus'
                     | 'GetChargingProfiles'
                     | 'GetCompositeSchedule'
                     | 'GetDisplayMessages'
                     | 'GetInstalledCertificateIds'
                     | 'GetLocalListVersion'
                     | 'GetLog'
                     | 'GetMonitoringReport'
                     | 'GetReport'
                     | 'GetTransactionStatus'
                     | 'GetVariables'
                     | 'Heartbeat'
                     | 'InstallCertificate'
                     | 'LogStatusNotification'
                     | 'MeterValues'
                     | 'NotifyChargingLimit'
                     | 'NotifyCustomerInformation'
                     | 'NotifyDisplayMessages'
                     | 'NotifyEVChargingNeeds'
                     | 'NotifyEVChargingSchedule'
                     | 'NotifyEvent'
                     | 'NotifyMonitoringReport'
                     | 'NotifyReport'
                     | 'PublishFirmware'
                     | 'PublishFirmwareStatusNotification'
                     | 'ReportChargingProfiles'
                     | 'RequestStartTransaction'
                     | 'RequestStopTransaction'
                     | 'ReservationStatusUpdate'
                     | 'ReserveNow'
                     | 'Reset'
                     | 'SecurityEventNotification'
                     | 'SendLocalList'
                     | 'SetChargingProfile'
                     | 'SetDisplayMessage'
                     | 'SetMonitoringBase'
                     | 'SetMonitoringLevel'
                     | 'SetNetworkProfile'
                     | 'SetVariableMonitoring'
                     | 'SetVariables'
                     | 'SignCertificate'
                     | 'StatusNotification'
                     | 'TransactionEvent'
                     | 'TriggerMessage'
                     | 'UnlockConnector'
                     | 'UnpublishFirmware'
                     | 'UpdateFirmware'.

-type payload() :: jerk:jerkterm().
-opaque message() :: {messageid(), payload()}.
-type messageid() :: binary().

-define(BASE_PATH, "urn:OCPP:Cp:2:2020:3").

%% Objective - when we return a value that is itself a jerk object, we
%% return a message fragment with an id equal to <<MessageId/binary,
%% "#/", AttributeName>>. (e.g. "1235412deadbeef#/chargingStation")

-record(msgid, {id :: binary(), path = [] :: [binary()]}).

-spec properties(message()) -> [binary()].
properties({_, Message}) ->
    jerk:attributes(Message).

%% @see new_request/3
-spec new_request(messagetype(), message_body()) -> message().
new_request(MessageType, MessageBody) ->
    new_request(MessageType, MessageBody, new_messageid()).

%% @doc Create a new request. If the properties in `MessageBody' are
%% invalid or do not satisfy the message schema the call will raise an
%% error with reason `badarg'.
-spec new_request(messagetype(), message_body(), messageid()) -> message().
new_request(MessageType, MessageBody, MessageId) ->
    make_message(MessageType, <<"Request">>, MessageBody, MessageId).

%% @doc Construct a new response term.
-spec new_response(messagetype(), message_body(), messageid()) -> message().
new_response(MessageType, MessageBody, MessageId) ->
    make_message(MessageType, <<"Response">>, MessageBody, MessageId).

make_message(MessageType, MessageClass, MessageBody, MessageId) ->
    MsgType = atom_to_binary(MessageType),
    SchemaURI = message_uri(<<MsgType/binary, MessageClass/binary>>),
    {MessageId, jerk:new(SchemaURI, prepare_payload(MessageBody))}.

message_type(Message, Class) ->
    MsgType = type(Message),
    {Pos, _} = binary:match(MsgType, Class),
    <<Type:Pos/binary, Class/binary>> = MsgType,
    binary_to_atom(Type).

-spec request_type(message()) -> messagetype().
request_type(Message) ->
    message_type(Message, <<"Request">>).

-spec response_type(message()) -> messagetype().
response_type(Message) ->
    message_type(Message, <<"Response">>).

-spec type(Message :: message()) -> binary().
type({_MessageId, Message}) ->
    Schema = jerk:id(Message),
    hd(lists:reverse(binary:split(Schema, <<":">>, [global]))).

-spec get(Key :: binary(), Message :: message()) -> jerk:primterm() | jerk:jerkterm().
get(Key, {MessageId, Message}) ->
    case get(string:split(Key, "/", all), decompose_msgid(MessageId), Message) of
        {SubId, Value} ->
            {SubId, Value};
        Value ->
            Value
    end.

decompose_msgid(MessageId) ->
    case binary:split(MessageId, <<"#/">>) of
        [MessageId] ->
            #msgid{id = MessageId};
        [MsgId, Path] ->
            #msgid{id = MsgId, path = binary:split(Path, <<"/">>, [global])}
    end.

recompose_msgid(#msgid{id = Id, path = []}) ->
    Id;
recompose_msgid(#msgid{id = Id, path = Path}) ->
    FullPath = join(Path, <<"/">>),
    <<Id/binary, "#/", FullPath/binary>>.

join([Bin|Binaries], Delimiter) ->
    lists:foldl(
      fun(X, Acc) ->
              <<Acc/binary, Delimiter/binary, X/binary>>
      end, <<Bin/binary>>, Binaries).

get([], MessageId, Value) ->
    case jerk:is_object(Value) of
        true ->
            {recompose_msgid(MessageId), Value};
        false ->
            Value
    end;
get([Key|Rest], #msgid{path = Path} = MsgId, IntermediatValue) ->
    get(Rest,
        MsgId#msgid{path = Path ++ [Key]},
        jerk:get_value(IntermediatValue, Key)).

%% @doc Return the message ID.
-spec id(Message :: message()) -> messageid().
id({Id, _}) -> Id.

message_uri(MessageType) when is_list(MessageType) ->
    message_uri(list_to_binary(MessageType));
message_uri(MessageType) when is_binary(MessageType) ->
    <<?BASE_PATH, ":", MessageType/binary>>.

%% transform a map into a key-value list and list-based strings
%% into binaries.
prepare_payload(Payload) when is_list(Payload) ->
    [ prepare_property(Property) || Property <- Payload];
prepare_payload(Payload) when is_map(Payload) ->
    prepare_payload(maps:to_list(Payload)).

prepare_property({K, V}) ->
    {to_binary(K), prepare_value(V)}.

to_binary(X) when is_list(X) ->
    list_to_binary(X);
to_binary(X) ->
    X.

prepare_value(V) when is_map(V) ->
    prepare_payload(V);
prepare_value(V) when is_list(V) ->
    case is_property_list(V) of
        true ->
            prepare_payload(V);
        false ->
            prepare_array(V)
    end;
prepare_value(V) ->
    V.

prepare_array([]) -> [];
prepare_array([H|T]) when is_map(H) -> [prepare_payload(H) | prepare_array(T)];
prepare_array([H|T]) -> [H | prepare_array(T)].


is_property_list([]) -> false;
is_property_list(List) ->
    lists:all(
      fun ({X, _}) when is_list(X)   -> io_lib:printable_unicode_list(X);
          ({X, _}) when is_binary(X) -> true;
          (_)                        -> false
      end,
      List).

new_messageid() ->
    list_to_binary(
      lists:flatten(
        io_lib:format("~.16b", [erlang:unique_integer([positive])]))).
