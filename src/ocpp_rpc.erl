%%% Simplified RPC framework for OCPP.
%%%
%%% Copyright (c) 2023 Will Vining <wfv@vining.dev>

-module(ocpp_rpc).

-export([call/3, callerror/2, callresult/2, decode/1]).

-export_type([messagetype/0, rpc_error/0, constraint_violation/0]).

-type messagetype() :: call
                     | callresult
                     | callerror.

-type constraint_violation() :: occurence_violation
                              | property_violation
                              | type_violation.

-type rpc_error() :: format_violation  % Payload for Action is syntactically incorrect
                   | generic_error     % a non-specific error
                   | internal_error
                   | not_implemented
                   | not_supported
                   | protocol_error
                   | rpc_error
                   | security_error
                   | constraint_violation().

-type messageid() :: string() | binary().

-define(MESSAGE_TYPE_CALL, 2).
-define(MESSAGE_TYPE_RESULT, 3).
-define(MESSAGE_TYPE_ERROR, 4).

-spec call(MessageId :: messageid(), Action :: binary(), Payload :: map()) ->
          {ok, Message :: binary()} |
          {error, Reason :: any()}.
call(MessageId, Action, Payload) ->
    encode_json([messagetype_to_id(call), MessageId, Action, Payload]).

%% @doc Return an encoded OCPP CALLERROR message with the given error reason.
-spec callerror(Error :: rpc_error(), MessageId :: messageid()) -> binary().
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

decode(MessageBinary) ->
    %% TODO Validate the response with the message schemas
    try
        [MessageTypeId, MessageId | Rest] = jiffy:decode(MessageBinary),
        %{ok, {id_to_messagetype(MessageTypeId), MessageId, Rest}},
        MessageType = id_to_messagetype(MessageTypeId),
        case validate(MessageType, Rest) of
            {ok, Message} -> {ok, {MessageType, MessageId, Message}};
            {error, _} = Error -> Error
        end
    catch
        error:_ ->
            {error, format_violation}
    end.

%%% Internal functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

encode_json(RPCMessage) -> jiffy:encode(RPCMessage).

validate(call, [Action, Payload]) ->
    ocpp_schema:validate(Action, Payload).

%%% Utility functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

messagetype_to_id(call) -> ?MESSAGE_TYPE_CALL;
messagetype_to_id(callresult) -> ?MESSAGE_TYPE_RESULT;
messagetype_to_id(callerror) -> ?MESSAGE_TYPE_ERROR;
messagetype_to_id(_) -> error(not_supported).

id_to_messagetype(?MESSAGE_TYPE_CALL) -> call;
id_to_messagetype(?MESSAGE_TYPE_RESULT) -> callresult;
id_to_messagetype(?MESSAGE_TYPE_ERROR) -> callerror;
id_to_messagetype(_) -> error(not_supported).

error_to_binary(format_violation) -> <<"FormatViolation">>;
error_to_binary(generic_error) -> <<"GenericError">>;
error_to_binary(internal_error) -> <<"InternalError">>;
error_to_binary(not_implemented) -> <<"NotImplemented">>;
error_to_binary(not_supported) -> <<"NotSupported">>;
error_to_binary(protocol_error) ->  <<"ProtocolError">>;
error_to_binary(rpc_error) -> <<"RpcFrameworkError">>;
error_to_binary(security_error) -> <<"SecurityError">>;
error_to_binary(occurence_violation) -> <<"OccurenceConstraintViolation">>;
error_to_binary(property_violation) -> <<"PropertyConstraintViolation">>;
error_to_binary(type_violation) -> <<"TypeConstraintViolation">>.
