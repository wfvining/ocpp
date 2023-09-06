%%% @doc OCPP Device Model implementation.
%%%
%%% @end
%%% Copyright (c) 2023 Will Vining <wfv@vining.dev>
-module(ocpp_device_model).

-export([new/0, add_variable/5, add_attribute/8, variable_exists/4, get_value/5]).

-export_type([device_model/0, attribute/0, charactersitics/0,
              variable_identifiers/0, attribute_type/0]).

-type attribute_type() :: ocpp_message:attribute_type().
-type attribute() ::
    #{type := attribute_type(),
      value := any(),
      mutability => [read | write],
      persistent => boolean()}.
-type charactersitics() ::
    #{unit => string(),
      maxlimit => any(),
      minlimit => any(),
      supports_monitoring := boolean(),
      datatype := ocpp_message:data_type(),
      valuelist => [binary()]}.
-type variable_identifiers() ::
    #{component_instance => binary(),
      variable_instance => binary(),
      evse => pos_integer(),
      connector => pos_integer()}.

-record(device_model,
        {characteristics = ets:new(characteristics, [bag, private, {keypos, 2}]) :: ets:table(),
         attributes = ets:new(attributes, [bag, private, {keypos, 2}]) :: ets:table()}).

-opaque device_model() :: #device_model{}.

-spec new() -> device_model().
new() ->
    #device_model{}.

%% @doc Add a variable to the device model by specifying its
%% characteristics. If the variable already exists `false' is
%% returned, otherwise returns `true'.
-spec add_variable(DeviceModel :: device_model(),
                   ComponentName :: binary(),
                   VariableName :: binary(),
                   VariableIdentifiers :: variable_identifiers(),
                   Characteristics :: charactersitics()) ->
                      boolean().
add_variable(DeviceModel,
             ComponentName,
             VariableName,
             VariableIdentifiers,
             Characteristics) ->
    ets:insert(DeviceModel#device_model.characteristics,
               {string:uppercase(ComponentName),
                string:uppercase(VariableName),
                uppercase_identifiers(VariableIdentifiers),
                Characteristics}).

-spec add_attribute(DeviceModel :: device_model(),
                    ComponentName :: binary(),
                    VariableName :: binary(),
                    VariableIdentifiers :: variable_identifiers(),
                    AttributeType :: attribute_type(),
                    AttributeValue :: binary() | undefined,
                    Mutability :: [read | write],
                    Persistent :: boolean()) ->
                       boolean().
add_attribute(DeviceModel,
              ComponentName,
              VariableName,
              VariableIdentifiers,
              AttributeType,
              AttributeValue,
              Mutability,
              Persistent) ->
    Type = get_type(DeviceModel, ComponentName, VariableName, VariableIdentifiers),
    Value = parse_value(Type, AttributeValue),
    ets:insert(DeviceModel#device_model.attributes,
               {string:uppercase(ComponentName),
                string:uppercase(VariableName),
                uppercase_identifiers(VariableIdentifiers),
                AttributeType,
                Value,
                Mutability,
                Persistent}).

uppercase_identifiers(VariableIdentifiers) ->
    uppercase_keys([variable_instance, component_instance], VariableIdentifiers).

uppercase_keys(Keys, Map) ->
    lists:foldl(fun(Key, Acc) -> uppercase_if_exists(Key, Acc) end, Map, Keys).

uppercase_if_exists(Key, Map) ->
    case maps:is_key(Key, Map) of
        true -> Map#{Key => string:uppercase(maps:get(Key, Map))};
        false -> Map
    end.

get_type(DeviceModel, ComponentName, VariableName, VariableIdentifiers) ->
    case ets:match(DeviceModel#device_model.characteristics,
                   {string:uppercase(ComponentName),
                    string:uppercase(VariableName),
                    uppercase_identifiers(VariableIdentifiers),
                    '$1'})
    of
        [[Characteristics]] ->
            maps:get(datatype, Characteristics);
        [] -> error(undefined);
        _ -> error(ambiguous)
    end.

%% @doc Return the value of the attribute of the given variable. If
%% the variable or attribute is undefined the call fails with reason `undefined'.
%% If the specified variable is ambiguous the call fails with reason `ambiguous'.
-spec get_value(DeviceModel :: device_model(),
                ComponentName :: binary(),
                VariableName :: binary(),
                VariableIdentifiers :: variable_identifiers(),
                AttributeType :: attribute_type()) ->
          any().
get_value(#device_model{attributes = Attributes},
          ComponentName,
          VariableName,
          VariableIdentifiers,
          AttributeType) ->
    case ets:match(Attributes,
                   {string:uppercase(ComponentName),
                    string:uppercase(VariableName),
                    uppercase_identifiers(VariableIdentifiers),
                    AttributeType,
                    '$1', '_', '_'})
    of
        [[Value]] -> Value;
        [] -> error(undefined);
        _ -> error(ambiguous)
    end.

parse_value(_, undefined) ->
    undefined;
parse_value(<<"string">>, Value) ->
    Value;
parse_value(<<"decimal">>, Value) ->
    binary_to_float(Value);
parse_value(<<"integer">>, Value) ->
    binary_to_integer(Value);
parse_value(<<"dateTime">>, Value) ->
    calendar:rfc3339_to_system_time(Value);
parse_value(<<"boolean">>, <<"true">>) ->
    true;
parse_value(<<"boolean">>, <<"false">>) ->
    false;
parse_value(_, Value) ->
    Value.

%% @doc Returns true if the identified variable exists in the device
%% model. If the `ComponentName', `VariableName', and
%% `VariableIdentifiers' match more than one variable the call fails
%% with reason `ambiguous'.
-spec variable_exists(DeviceModel :: device_model(),
                      ComponentName :: binary(),
                      VariableName :: binary(),
                      VariableIdentifiers :: variable_identifiers()) -> boolean().
variable_exists(#device_model{characteristics = Characteristics},
                ComponentName, VariableName, VariableIdentifiers) ->
    case ets:match_object(Characteristics,
                          {string:uppercase(ComponentName),
                           string:uppercase(VariableName),
                           uppercase_identifiers(VariableIdentifiers),
                           '_'})
    of
        [] -> false;
        [_Var] -> true;
        _ -> error(ambiguous)
    end.
