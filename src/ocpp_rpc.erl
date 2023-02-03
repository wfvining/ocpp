%%% Simplified RPC framework for OCPP.
%%%
%%% Copyright (c) 2022 William Vining <wfv@vining.dev>

-module(ocpp_rpc).

-export([call/4]).

-type messagetype() :: call
                     | callresult
                     | callerror.

-type rpc_error() :: format_violation
                   | generic_error
                   | internal_error
                     . %% | ...

-type server() :: any().

-spec call(Server :: server(), MessageId :: string(), Action :: string(), Payload :: binary()) ->
          {ok, Result :: #{binary() => any()}} |
          {error, ErrorCode :: rpc_error(), ErrorDescription :: binary(), ErrrorDetails :: #{binary() => any()}}.
call(Server, MessageId, Action, Payload) ->
    Message = encode(call, MessageId, [Action, Payload]),
    ok = send(Server, Message),
    receive_result(Server, MessageId).

%%% Internal functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

encode_json(RPCMessage) -> jiffy:encode(RPCMessage).

send(Server, Message) ->
    io:format("[sending message to ~p] ~p~n", [Server, Message]).

encode(MessageType, MessageId, Rest) ->
    encode_json([messagetype_to_id(MessageType), MessageId | Rest]).

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

validate(call, [Action, Payload]) ->
    ocpp_schema:validate(Action, Payload).

receive_result(Server, MessageId) ->
    error('not implemented').

%%% Utility functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

messagetype_to_id(call) -> 2;
messagetype_to_id(callresult) -> 3;
messagetype_to_id(callerror) -> 4;
messagetype_to_id(_) -> error(not_supported).

id_to_messagetype(2) -> call;
id_to_messagetype(3) -> callresult;
id_to_messagetype(4) -> callerror;
id_to_messagetype(_) -> error(not_supported).
