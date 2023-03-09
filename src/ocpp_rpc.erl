%%% Simplified RPC framework for OCPP.
%%%
%%% Copyright (c) 2023 Will Vining <wfv@vining.dev>

-module(ocpp_rpc).

-export([call/3, callerror/2, callresult/2, decode/1]).

-export_type([messagetype/0, rpcerror/0, constraint_violation/0,
              request/0, response/0]).

-type messagetype() :: call
                     | callresult
                     | callerror.

%% TODO
-type request() :: any().
-type response() :: any().

-type constraint_violation() :: occurence_violation
                              | property_violation
                              | type_violation.

-type payload_error() :: format_violation %% payload for `Action' syntactically incorrect
                       | generic_error
                       | internal_error
                       | not_implemented
                       | not_supported
                       | protocol_error
                       | security_error
                       | constraint_violation().

-type framework_error() :: rpc_framework_error %% Not a valid RPC request
                         | message_type_not_supported. %% Message type number not supported

-type rpcerror() :: payload_error()
                  | framework_error().

-type decode_error() :: rpc_framework_error
                      | {message_type_not_supported, MessageTypeId :: integer()}
                      | {payload_error(), MessageId :: messageid()}.

-type messageid() :: string() | binary().

-define(MESSAGE_TYPE_CALL, 2).
-define(MESSAGE_TYPE_RESULT, 3).
-define(MESSAGE_TYPE_ERROR, 4).

-spec call(MessageId :: messageid(), Action :: binary(), Payload :: map()) ->
          {ok, Message :: binary()} |
          {error, Reason :: any()}.
call(MessageId, Action, Payload) ->
    encode_json([messagetype_to_id(call), MessageId, Action, Payload]).

%% @doc Return an encoded RPC error message with the given error reason.
-spec callerror(Error :: rpcerror(), MessageId :: messageid()) -> binary().
callerror(Error, MessageId) ->
    CallError = make_callerror(Error, MessageId),
    encode_json(CallError).

-spec callresult(MessageId :: messageid(), Payload :: map()) -> binary().
callresult(MessageId, Payload) ->
    encode_json([messagetype_to_id(callresult), MessageId, Payload]).

make_callerror(Error, MessageId) ->
    make_callerror(Error, MessageId, <<"">>, #{}).

make_callerror(Error, MessageId, ErrorDescription, ErrorDetails) ->
    [messagetype_to_id(callerror),
     MessageId,
     error_to_binary(Error),
     ErrorDescription,
     ErrorDetails].

-spec decode(MessageBinary :: binary()) ->
          {ok, {messagetype(), messageid(), jiffy:json_value()}} |
          {error, format_violation} |
          {error, {message_type_not_supported, MessageTypeId :: integer()}} |
          {error, {rpcerror(), MessageId :: messageid()}}.
decode(MessageBinary) ->
    try
        [MessageTypeId, MessageId | Rest] = jiffy:decode(MessageBinary),
        case id_to_messagetype(MessageTypeId) of
            {ok, MessageType} ->
                case validate(MessageType, Rest) of
                    {ok, Message} -> {ok, {MessageType, MessageId, Message}};
                    {error, _} = Error -> Error
                end;
            {error, {message_type_not_supported, _}} = Error -> Error
        end
    catch
        error:_ ->
            {error, format_violation}
    end.

%%% Internal functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

encode_json(RPCMessage) -> jiffy:encode(RPCMessage).

validate(call, [Action, Payload]) ->
    case ocpp_schema:validate(Action, Payload) of
        ok ->
            {ok, {Action, Payload}};
        {error, _} = Error -> Error
    end.

%%% Utility functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

messagetype_to_id(call) -> ?MESSAGE_TYPE_CALL;
messagetype_to_id(callresult) -> ?MESSAGE_TYPE_RESULT;
messagetype_to_id(callerror) -> ?MESSAGE_TYPE_ERROR.

id_to_messagetype(?MESSAGE_TYPE_CALL) -> {ok, call};
id_to_messagetype(?MESSAGE_TYPE_RESULT) -> {ok, callresult};
id_to_messagetype(?MESSAGE_TYPE_ERROR) -> {ok, callerror};
id_to_messagetype(MessageTypeId) -> {error, {message_type_not_supported, MessageTypeId}}.

error_to_binary(format_violation) -> <<"FormatViolation">>;
error_to_binary(generic_error) -> <<"GenericError">>;
error_to_binary(internal_error) -> <<"InternalError">>;
error_to_binary(not_implemented) -> <<"NotImplemented">>;
error_to_binary(message_type_not_supported) -> <<"MessageTypeNotSupported">>;
error_to_binary(not_supported) -> <<"NotSupported">>;
error_to_binary(protocol_error) ->  <<"ProtocolError">>;
error_to_binary(rpc_framework_error) -> <<"RpcFrameworkError">>;
error_to_binary(security_error) -> <<"SecurityError">>;
error_to_binary(occurence_violation) -> <<"OccurenceConstraintViolation">>;
error_to_binary(property_violation) -> <<"PropertyConstraintViolation">>;
error_to_binary(type_violation) -> <<"TypeConstraintViolation">>.
