-module(ocpp_message).

-export([new_request/2, new_request/3, new_response/3,
         properties/1, get/2, get/3, id/1,
         type/1, request_type/1, response_type/1]).

-type message_body() :: #{binary() | string() => jerk:primterm() | message_body()}
                      | [{binary() | string(), jerk:primterm() | message_body()}].

-export_type([message/0, messageid/0, messagetype/0]).

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

%%% ========= OCPP Message-Related Types =========

-export_type([boot_request/0, set_variables_request/0]).
-export_type([boot_response/0, set_variables_response/0]).

-export_type([boot_status/0, status_info/0, boot_reason/0,
              modem/0, charging_station/0]).

-type custom_data() :: #{atom() | binary() => any()}.

-type boot_status() :: 'Accepted' | 'Rejected' | 'Pending'.

-type status_info() :: #{'reasonCode' := binary(), 'additionalInfo' => binary()}.

-type boot_response() ::
        #{'status' := boot_status(),
          'interval' := non_neg_integer(),
          'currentTime' := calendar:datetime(),
          'statusInfo' => status_info()}.

-type boot_reason() :: 'ApplicationReset'
                     | 'FirmwareUpdate'
                     | 'LocalReset'
                     | 'PowerUp'
                     | 'RemoteReset'
                     | 'ScheduledReset'
                     | 'Triggered'
                     | 'Unknown'
                     | 'Watchdog'.

-type modem() :: #{'iccid' => binary(), 'imsi' => binary()}.

-type charging_station() ::
        #{'model' := binary(),
          'vendorName' := binary(),
          'firmwareVersion' => binary(),
          'serialNumber' => binary(),
          'modem' => modem()}.

-type boot_request() ::
        #{'reason' := boot_reason(),
          'chargingStation' := charging_station(),
          'customData' => custom_data()}.

-type attribute_type() :: 'Actual' | 'Target' | 'MinSet' | 'MaxSet'.

-type evse() :: #{'id' := pos_integer(), 'connectorId' => pos_integer()}.

-type component() ::
        #{'name' := binary(),
          'instance' => binary(),
          'evse' => evse()}.

-type variable() :: #{'name' := binary(), 'instance' => binary()}.

-type set_variable_data() ::
        #{'attributeType' => attribute_type(),
          'attributeValue' := binary(),
          'component' := component(),
          'variable' := variable()}.

-type set_variables_request() ::
        #{'setVariableData' := [set_variable_data()],
          'customData' => custom_data}.

-type set_variable_status() ::
        'Accepted' |
        'Rejected' |
        'UnknownComponent' |
        'UnknownVariable' |
        'NotSupportedAttribute' |
        'RebootRequired'.

-type set_variable_result() ::
        #{'attributeType' => attribute_type(),
          'attributeStatus' := set_variable_status(),
          'component' := component(),
          'variable' := variable(),
          'attributeStatusInfo' => status_info()}.

-type set_variables_response() ::
        #{'setVariableResult' := [set_variable_result()],
          'customData' => custom_data()}.

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
    get(string:split(Key, "/", all), decompose_msgid(MessageId), Message, []).

-spec get(Key :: binary(), Message :: message(), Default) -> jerk:primterm() | jerk:jerkterm() | Default.
get(Key, {MessageId, Message}, Default) ->
    get(string:split(Key, "/", all), decompose_msgid(MessageId), Message, [{default, Default}]).

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

get([], #msgid{path = Path} = MessageId, Value, Options) when is_list(Value) ->
    [get([], MessageId#msgid{path = Path ++ [<<"$array-element">>]}, X, Options)
     || X <- Value];
get([], MessageId, Value, _) ->
    case jerk:is_object(Value) of
        true ->
            {recompose_msgid(MessageId), Value};
        false ->
            Value
    end;
get([Key|Rest], #msgid{path = Path} = MsgId, IntermediatValue, Options) ->
    try
        get(Rest,
            MsgId#msgid{path = Path ++ [Key]},
            jerk:get_value(IntermediatValue, Key),
            Options)
    catch error:{undefined, _} = E ->
            HasDefault = lists:member(default, proplists:get_keys(Options)),
            if HasDefault ->
                    proplists:get_value(default, Options);
               not HasDefault ->
                    error(E)
            end
    end.

%% @doc Return the message ID.
-spec id(Message :: message()) -> messageid().
id({Id, _}) -> Id.

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

to_binary(X) when is_atom(X) ->
    atom_to_binary(X);
to_binary(X) when is_list(X) ->
    list_to_binary(X);
to_binary(X) ->
    X.

prepare_value(V) when is_boolean(V) ->
    V;
prepare_value(V) when is_atom(V) ->
    prepare_value(atom_to_binary(V));
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
