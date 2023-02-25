%%% Tools for OCPP JSON schema validation.
%%%
%%% Copyright (c) 2023 William Vining <wfv@vining.dev>
-module(ocpp_schema).

-export([init_schemas/1, validate/2]).

-spec init_schemas(SchemaDir :: file:name()) -> ok | {error, Reason :: any()}.
init_schemas(SchemaDir) ->
    jesse:load_schemas(SchemaDir, fun jiffy:decode/1).

-spec validate(ActionName :: binary(), Payload :: jesse:json_term()) ->
          ok |
          {error, Reason :: any()}.
validate(ActionName, Payload) ->
    Schema = binary_to_list(ActionName),
    case jesse:validate(Schema, Payload) of
        {ok, _} -> ok;
        Error -> Error
    end.
