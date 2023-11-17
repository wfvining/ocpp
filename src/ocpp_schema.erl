%%% Tools for OCPP JSON schema validation.
%%%
%%% Copyright (c) 2023 William Vining <wfv@vining.dev>
-module(ocpp_schema).

-export([init_schemas/1, validate/2]).

-type validation_error() :: bad_property
                          | invalid_payload
                          | bad_type
                          | bad_value
                          | unknown_action.

-spec init_schemas(SchemaDir :: file:name()) -> ok | {error, Reason :: any()}.
init_schemas(SchemaDir) ->
    {ok, Files} = file:list_dir(SchemaDir),
    SchemaFiles = [F || F <- Files, filename:extension(F) =:= ".json"],
    [ok = jerk:load_schema(filename:join(SchemaDir, File)) || File <- SchemaFiles],
    jesse:load_schemas(SchemaDir, fun jiffy:decode/1).

-spec validate(ActionName :: binary(), Payload :: jesse:json_term()) ->
          ok | {error, Reason :: validation_error()}.
validate(ActionName, Payload) ->
    SchemaURN = schema_urn(ActionName),
    case jesse:validate(SchemaURN, Payload) of
        {ok, _} -> ok;
        {error, {database_error, _, schema_not_found}} ->
            {error, unknown_action};
        {error, [Error]} -> {error, validation_error(Error)}
    end.

schema_urn(Action) ->
    "urn:OCPP:Cp:2:2020:3:" ++ binary_to_list(Action).

%% XXX The atoms that define jesse error reasons are not fully documented.
validation_error({data_invalid, _Schema,
                  Reason, _, _})
  when Reason =:= missing_required_property;
       Reason =:= no_extra_properties_allowed ->
    bad_property;
validation_error({data_invalid, _Schema,
                  wrong_type, _, []}) ->
    invalid_payload;
validation_error({data_invalid, _Schema,
                  Reason, _, [_|_]})
  when Reason =:= wrong_type;
       Reason =:= not_in_enum;
       Reason =:= not_array ->
    bad_type;
validation_error({data_invalid, _Schema,
                  _Reason, _, [_|_]}) ->
    %% Assume every other reason is a bad value.
    bad_value.
