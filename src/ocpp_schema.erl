%%% Tools for OCPP JSON schema validation.
%%%
%%% Copyright (c) 2023 William Vining <wfv@vining.dev>
-module(ocpp_schema).

-export([init_schemas/1, validate/2]).

-spec init_schemas(SchemaDir :: file:name()) -> ok | {error, Reason :: any()}.
init_schemas(SchemaDir) ->
    case file:list_dir(SchemaDir) of
        {ok, FileNames} ->
            lists:foreach(
              fun (FileName) -> load_schema(SchemaDir, FileName) end,
              FileNames);
        {error, Reason} -> {error, Reason}
    end.

load_schema(SchemaDir, FileName) ->
    SchemaName = filename:rootname(filename:basename(FileName)),
    Schema = read_schema(filename:join([SchemaDir, FileName])),
    jesse:add_schema(SchemaName, Schema).

read_schema(SchemaPath) ->
    {ok, Data} = file:read_file(SchemaPath),
    jiffy:decode(Data).

-spec validate(ActionName :: binary(), Payload :: jesse:json_term()) ->
          ok |
          {error, Reason :: any()}.
validate(ActionName, Payload) ->
    Schema = binary_to_list(ActionName),
    case jesse:validate(Schema, Payload) of
        {ok, _} -> ok;
        Error -> Error
    end.
