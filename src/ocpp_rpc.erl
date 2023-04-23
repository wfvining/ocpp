%%% Simplified RPC framework for OCPP.
%%%
%%% Copyright (c) 2023 Will Vining <wfv@vining.dev>

-module(ocpp_rpc).

-export([call/3, callerror/1, callerror/2, callresult/2, decode/1]).

-export_type([rpctype/0, rpcerror/0, constraint_violation/0,
              message/0]).

-type rpctype() :: call
                 | callresult
                 | callerror.

-type message() :: {rpctype(), messageid(), ocpp_message:message()}.

-type constraint_violation() :: occurrence_violation
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

%% @doc Construct an ocpp callerror from an error value returned by `decode/1'.
-spec callerror(Error :: decode_error()) -> binary().
callerror(rpc_framework_error = Error) ->
    callerror(Error, <<"-1">>);
callerror({message_type_not_supported = Error, MessageType}) ->
    make_callerror(
      Error, <<"-1">>, <<"Message type not supported">>,
      #{<<"MessageType">> => MessageType,
        <<"SupportedMessageTypes">> => [messagetype_to_id(Type)
                                        || Type <- [callerror, callresult, call]]});
callerror({Error, MessageId}) ->
    callerror(Error, MessageId).


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
          {ok, message()} |
          {error, decode_error()}.
decode(MessageBinary) ->
    DecodePipeline =
        bind([fun parse_json/1,
              fun decode_message_type/1,
              fun check_message_id/1,
              fun make_message/1]),
    DecodePipeline(MessageBinary).

%%% Internal functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_message([call, MessageId, Action, Payload]) when is_binary(Action) ->
    case validate(<<Action/binary, "Request">>, Payload) of
        {ok, _Payload} ->
            {ok, {call, MessageId, ocpp_message:request(Action, Payload)}};
        {error, Reason} ->
            {error, {Reason, MessageId}}
    end;
make_message([callresult, MessageId, Payload]) ->
    {ok, {response, MessageId, Payload}};
make_message(_) ->
    {error, rpc_framework_error}.

check_message_id([_, MessageId | _] = Msg) ->
    case catch string:length(MessageId) of
        {'EXIT', _} ->
            {error, rpc_framework_error};
        N when N =< 36 ->
            {ok, Msg};
        N when N > 36 ->
            {error, rpc_framework_error}
    end;
check_message_id(_) ->
    {error, rpc_framework_error}.

decode_message_type([MessageTypeId|Rest]) ->
    case id_to_messagetype(MessageTypeId) of
        {ok, MessageType} ->
            {ok, [MessageType | Rest]};
        {error, _} = Error -> Error
    end;
decode_message_type(_) ->
    {error, rpc_framework_error}.

parse_json(Binary) ->
    try {ok, jiffy:decode(Binary, [return_maps])}
    catch
        error:_ ->
            {error, rpc_framework_error}
    end.

bind(Funs) ->
    fun (Input) -> pipe({ok, Input}, Funs) end.

pipe(Input, []) -> Input;
pipe({error, _} = Error, _) -> Error;
pipe({ok, Input}, [F|Funs]) -> pipe(F(Input), Funs).

encode_json(RPCMessage) -> jiffy:encode(RPCMessage).

validate(Action, Payload) ->
    case ocpp_schema:validate(Action, Payload) of
        ok ->
            {ok, {Action, Payload}};
        {error, unknown_action} ->
            {error, not_implemented};
        {error, invalid_payload} ->
            {error, protocol_error};
        {error, bad_property} ->
            {error, occurrence_violation};
        {error, bad_type} ->
            {error, type_violation};
        {error, bad_value} ->
            {error, property_violation}
    end.

%%% Utility functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

messagetype_to_id(call) -> ?MESSAGE_TYPE_CALL;
messagetype_to_id(callresult) -> ?MESSAGE_TYPE_RESULT;
messagetype_to_id(callerror) -> ?MESSAGE_TYPE_ERROR.

id_to_messagetype(?MESSAGE_TYPE_CALL) -> {ok, call};
id_to_messagetype(?MESSAGE_TYPE_RESULT) -> {ok, callresult};
id_to_messagetype(?MESSAGE_TYPE_ERROR) -> {ok, callerror};
id_to_messagetype(MessageTypeId) when is_integer(MessageTypeId) ->
    {error, {message_type_not_supported, MessageTypeId}};
id_to_messagetype(_) -> {error, rpc_framework_error}.

error_to_binary(format_violation) -> <<"FormatViolation">>;
error_to_binary(generic_error) -> <<"GenericError">>;
error_to_binary(internal_error) -> <<"InternalError">>;
error_to_binary(not_implemented) -> <<"NotImplemented">>;
error_to_binary(message_type_not_supported) -> <<"MessageTypeNotSupported">>;
error_to_binary(not_supported) -> <<"NotSupported">>;
error_to_binary(protocol_error) ->  <<"ProtocolError">>;
error_to_binary(rpc_framework_error) -> <<"RpcFrameworkError">>;
error_to_binary(security_error) -> <<"SecurityError">>;
error_to_binary(occurrence_violation) -> <<"OccurrenceConstraintViolation">>;
error_to_binary(property_violation) -> <<"PropertyConstraintViolation">>;
error_to_binary(type_violation) -> <<"TypeConstraintViolation">>.
