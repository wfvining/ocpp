%%% @doc Simple API for working with OCPP Errors.
%%% @end
%%%
%%% Copyright (c) 2023 Will Vining <wfv@vining.dev>

-module(ocpp_error).

-export([new/2, new/3, id/1, type/1, code/1, description/1, details/1]).

-export_type([code/0, error/0, jsonterm/0]).

-type code() :: 'FormatViolation'
              | 'GenericError'
              | 'InternalError'
              | 'MessageTypeNotSupported'
              | 'NotImplemented'
              | 'NotSupported'
              | 'OccurrenceConstraintViolation'
              | 'PropertyConstraintViolation'
              | 'ProtocolError'
              | 'RpcFrameworkError'
              | 'SecurityError'
              | 'TypeConstraintViolation'.

-type jsonterm() :: binary()
                  | number()
                  | boolean()
                  | null
                  | [jsonterm()]
                  | #{binary() => jsonterm()}.

-record(error, {id :: binary(),
                code :: code(),
                description :: undefined | binary(),
                details :: #{binary() => jsonterm()}}).

-opaque error() :: #error{}.

%% @equiv new(Code, MessageId, [])
-spec new(Code :: code(), MessageId :: binary()) -> error().
new(Code, MessageId) -> new(Code, MessageId, []).

%% @doc Create a new error.
-spec new(Code :: code(),
          MessageId :: binary(),
          Options :: [Option]) -> error()
              when Option :: {description, binary()}
                           | {details, #{binary() => jsonterm()}}.
new(Code, MessageId, Options) ->
    Description = proplists:get_value(description, Options, undefined),
    Details = proplists:get_value(details, Options, #{}),
    #error{id = MessageId,
           code = Code,
           description = Description,
           details = Details}.

%% @doc Return the Message Id this error is associated with.
-spec id(error()) -> binary().
id(#error{id = Id}) -> Id.

-spec code(error()) -> code().
code(#error{code = Code}) -> Code.

%% @doc Return the error code.
-spec type(error()) -> binary().
type(#error{code = Code}) -> atom_to_binary(Code).

%% @doc Return the error description. If one was not specified a
%% default description from the OCPP standard is returned.
-spec description(error()) -> binary().
description(#error{ code = Code, description = undefined }) ->
    default_description(Code);
description(#error{ description = Description}) ->
    Description.

%% @doc Return the error details.
-spec details(error()) -> #{binary() => any()}.
details(#error{details = Details}) ->
    Details.

default_description('FormatViolation') ->
    <<"Payload for Action is syntactically incorrect">>;
default_description('GenericError') ->
    <<"An error occured">>;
default_description('InternalError') ->
    <<"An internal error occurred and the receiver was not able "
      "to process the requested Action successfully">>;
default_description('MessageTypeNotSupported') ->
    <<"A message with an Message Type Number received that "
      "is not supported by this implementation">>;
default_description('NotImplemented') ->
    <<"Requested Action is not known by receiver">>;
default_description('NotSupported') ->
    <<"Requested Action is recognized but not supported by the receiver">>;
default_description('OccurrenceConstraintViolation') ->
    <<"Payload for Action is syntactically correct but at least "
      "one of the fields violates occurrence constraints">>;
default_description('PropertyConstraintViolation') ->
    <<"Payload is syntactically correct but at least "
      "one field contains an invalid value">>;
default_description('ProtocolError') ->
    <<"Payload for Action is not conform the PDU structure">>;
default_description('RpcFrameworkError') ->
    <<"Content of the call is not a valid RPC Request, for example: "
      "MessageId could not be read.">>;
default_description('SecurityError') ->
    <<"During the processing of Action a security issue occurred preventing "
      "receiver from completing the Action successfully">>;
default_description('TypeConstraintViolation') ->
    <<"Payload for Action is syntactically correct but at least one of "
      "the fields violates data type constraints (e.g. \"somestring\": 12)">>.
