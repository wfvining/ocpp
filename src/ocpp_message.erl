-module(ocpp_message).

-export([new/2, new/3, properties/1, get/2, get/3, id/1, type/1]).

-type payload() :: #{binary() | string() => jerk:primterm() | payload()}
                 | [{binary() | string(), jerk:primterm() | payload()}].

-export_type([message/0, messageid/0]).

-opaque message() :: {messageid(), jerk:jerkterm()}.
-type messageid() :: binary().

-define(BASE_PATH, "urn:OCPP:Cp:2:2020:3").

-spec properties(message()) -> [binary()].
properties({_, Message}) ->
    jerk:attributes(Message).

%% Objective - when we return a value that is itself a jerk object, we
%% return a message fragment with an id equal to <<MessageId/binary,
%% "#/", AttributeName>>. (e.g. "1235412deadbeef#/chargingStation")

-record(msgid, {id :: binary(), path = [] :: [binary()]}).

-spec type(Message :: message()) -> binary().
type({_MessageId, Message}) ->
    Schema = jerk:id(Message),
    hd(lists:reverse(binary:split(Schema, <<":">>, [global]))).

-spec get(Key :: binary(), Message :: message()) -> jerk:primterm() | jerk:jerkterm().
get(Key, {MessageId, Message}) ->
    get(string:split(Key, "/", all), decompose_msgid(MessageId), Message).

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

%% @doc @see new/3
-spec new(MessageType :: binary(), Payload :: payload()) -> message().
new(MessageType, Payload) ->
    new(MessageType, Payload, new_messageid()).

%% @doc Create a new message. If the properties in `Payload' are
%% invalid the call will fail with reason `badarg'
-spec new(MessageType :: binary(), Payload :: payload(), MessageId :: messageid()) ->
          message().
new(MessageType, Payload, MessageId) when is_list(MessageId) ->
    new(MessageType, Payload, list_to_binary(MessageId));
new(MessageType, Payload, MessageId) when is_binary(MessageId) ->
    {MessageId, jerk:new(message_uri(MessageType), prepare_payload(Payload))}.

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
            V
    end;
prepare_value(V) ->
    V.

is_property_list([]) -> false;
is_property_list(List) ->
    lists:all(
      fun ({X, _}) when is_list(X)   -> io_lib:printable_unicode_list(X);
          ({X, _}) when is_binary(X) -> true;
          (_)                        -> false
      end,
      List).

new_messageid() ->
    %% TODO replace with a uuid library
    ref_to_list(erlang:make_ref()).
