-module(ocpp_message).

-export([new/2, properties/1, get/2, get/3]).

-type payload() :: #{string() => jerk:primterm() | payload()}
                 | [{string(), jerk:primterm() | payload()}].

-export_type([message/0, messageid/0]).

-opaque message() :: {messageid(), jerk:jerkterm()}.
-type messageid() :: string().

-define(BASE_PATH, "urn:OCPP:Cp:2:2020:3").

-spec properties(message()) -> [binary()].
properties({_, Message}) ->
    jerk:attributes(Message).

-spec get(Key :: binary(), Message :: message()) -> jerk:primterm() | jerk:jerkterm().
get(Key, {_, Message}) ->
    get_value(string:split(Key, "/", all), Message).

get_value([], Value) -> Value;
get_value([Key|Rest], IntermediateValue) ->
    get_value(Rest, jerk:get_value(IntermediateValue, Key)).

-spec get(Key :: binary(), Message :: message(), Default) ->
          jerk:primterm() | jerk:jerkterm() | Default.
get(Key, {_, Message} = Msg, Default) ->
    case lists:member(Key, jerk:attributes(Message)) of
        true ->
            get(Key, Msg);
        false ->
            Default
    end.

%% @doc @see new/3
-spec new(MessageType :: string(), Payload :: payload()) -> message().
new(MessageType, Payload) ->
    new(MessageType, Payload, new_messageid()).

%% @doc Create a new message. If the properties in `Payload' are
%% invalid the call will fail with reason `badarg'
-spec new(MessageType :: string(), Payload :: payload(), MessageId :: messageid()) ->
          message().
new(MessageType, Payload, MessageId) ->
    {MessageId, jerk:new(message_uri(MessageType), prepare_payload(Payload))}.

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
