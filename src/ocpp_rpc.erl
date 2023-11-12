%%% Simplified RPC framework for OCPP.
%%%
%%% Copyright (c) 2023 Will Vining <wfv@vining.dev>

-module(ocpp_rpc).

-export([call/3, encode_callerror/1, encode_callresult/1, decode/1]).

-export_type([rpctype/0, message/0]).

-type rpctype() :: call
                 | callresult
                 | callerror.

-type message() :: {rpctype(), messageid(), ocpp_message:messagetype(), ocpp_message:message()}.

-type messageid() :: string() | binary().

-define(MESSAGE_TYPE_CALL, 2).
-define(MESSAGE_TYPE_RESULT, 3).
-define(MESSAGE_TYPE_ERROR, 4).

-spec call(MessageId :: messageid(), Action :: binary(), Payload :: map()) ->
          message().
call(MessageId, Action, Payload) ->
    {call, MessageId, Action, Payload}.

%% @doc Construct an ocpp callerror from an error value returned by `decode/1'.
-spec encode_callerror(Error :: ocpp_error:error()) -> binary().
encode_callerror(Error) ->
    encode_json(make_callerror(ocpp_error:code(Error),
                               ocpp_error:id(Error),
                               ocpp_error:description(Error),
                               ocpp_error:details(Error))).

-spec encode_callresult(ocpp_message:message()) -> binary().
encode_callresult(Message) ->
    encode_json([?MESSAGE_TYPE_RESULT,
                 ocpp_message:id(Message),
                 ocpp_message:to_map(Message)]).

make_callerror(Error, MessageId, ErrorDescription, ErrorDetails) ->
    [?MESSAGE_TYPE_ERROR,
     MessageId,
     atom_to_binary(Error),
     ErrorDescription,
     ErrorDetails].

-spec decode(MessageBinary :: binary()) ->
          {ok, message()} |
          {error, ocpp_error:error()}.
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
            {ok, {call, MessageId, ocpp_message:new_request(binary_to_atom(Action), Payload)}};
        {error, Reason} ->
            {error, ocpp_error:new(Reason, MessageId)}
    end;
make_message([call, MessageId, _, _]) ->
    {error, ocpp_error:new('RpcFrameworkError', MessageId)};
make_message([callresult, MessageId, Payload]) ->
    {ok, {callresult, MessageId, Payload}};
make_message(_) ->
    {error, ocpp_error:new('RpcFrameworkError', <<"-1">>)}.

check_message_id([_, MessageId | _] = Msg) ->
    case catch string:length(MessageId) of
        {'EXIT', _} ->
            {error, ocpp_error:new('RpcFrameworkError', <<"-1">>)};
        N when N =< 36 ->
            {ok, Msg};
        N when N > 36 ->
            {error, ocpp_error:new('RpcFrameworkError', <<"-1">>)}
    end;
check_message_id(_) ->
    {error, ocpp_error:new('RpcFrameworkError', <<"-1">>)}.

decode_message_type([MessageTypeId|Rest]) ->
    case id_to_messagetype(MessageTypeId) of
        {ok, MessageType} ->
            {ok, [MessageType | Rest]};
        {error, Reason} ->
            {error, ocpp_error:new(Reason, <<"-1">>)}
    end;
decode_message_type(_) ->
    {error, ocpp_error:new('RpcFrameworkError', <<"-1">>)}.

parse_json(Binary) ->
    try {ok, jiffy:decode(Binary, [return_maps])}
    catch
        error:_ ->
            {error, ocpp_error:new('RpcFrameworkError', <<"-1">>)}
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
            {error, 'NotImplemented'};
        {error, invalid_payload} ->
            {error, 'ProtocolError'};
        {error, bad_property} ->
            {error, 'OccurrenceConstraintViolation'};
        {error, bad_type} ->
            {error, 'TypeConstraintViolation'};
        {error, bad_value} ->
            {error, 'PropertyConstraintViolation'}
    end.

%%% Utility functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

id_to_messagetype(?MESSAGE_TYPE_CALL) -> {ok, call};
id_to_messagetype(?MESSAGE_TYPE_RESULT) -> {ok, callresult};
id_to_messagetype(?MESSAGE_TYPE_ERROR) -> {ok, callerror};
id_to_messagetype(MessageTypeId) when is_integer(MessageTypeId) ->
    {error, 'MessageTypeNotSupported'};
id_to_messagetype(_) -> {error, 'RpcFrameworkError'}.
