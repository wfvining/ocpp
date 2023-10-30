%%% @doc OCPP Device Model implementation.
%%%
%%% @end
%%% Copyright (c) 2023 Will Vining <wfv@vining.dev>
-module(ocpp_device_model).

-feature(maybe_expr, enable).

-export([new/0, add_variable/5, add_attribute/8, get_value/5, update_attribute/6]).

-export_type([device_model/0, attribute/0, charactersitics/0,
              variable_identifiers/0, attribute_type/0]).

-include_lib("stdlib/include/ms_transform.hrl").

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
      valuelist => [string()]}.
-type variable_identifiers() ::
    #{component_instance => string(),
      variable_instance => string(),
      evse => pos_integer(),
      connector => pos_integer()}.

-record(device_model,
        {characteristics = ets:new(characteristics, [set, private]) :: ets:table(),
         attributes = ets:new(attributes, [set, private]) :: ets:table()}).

-opaque device_model() :: #device_model{}.

-spec new() -> device_model().
new() ->
    M = #device_model{},
    add_defaults(M#device_model.characteristics),
    M.

add_defaults(Characteristics) ->
    Defaults = default_variable_characteristics(),
    lists:foreach(
      fun (Var) ->
              ets:insert(Characteristics, make_characteristics(Var))
      end, Defaults).

make_characteristics({Component, Variable, Characteristics}) ->
    {{prepare_component(Component), prepare_variable(Variable)}, maps:from_list(Characteristics)}.

uppercase(X) when is_atom(X) ->
    X;
uppercase(X) when is_list(X) ->
    string:uppercase(X);
uppercase(X) when is_binary(X) ->
    uppercase(binary_to_list(X)).

prepare_component({Name, EVSE, Connector, Instance}) ->
    {uppercase(Name), EVSE, Connector, uppercase(Instance)};
prepare_component({Name, EVSE, Connector}) ->
    {uppercase(Name), EVSE, Connector, none};
prepare_component({Name, EVSE}) ->
    {uppercase(Name), EVSE, none, none};
prepare_component(Name) ->
    {uppercase(Name), none, none, none}.

prepare_variable({Name, Instance}) ->
    {uppercase(Name), uppercase(Instance)};
prepare_variable(Name) ->
    {uppercase(Name), none}.

characteristics(#device_model{characteristics = Characteristics},
                ComponentName, VariableName, Options) ->
    EVSE = proplists:get_value(evse, Options, none),
    Connector = proplists:get_value(connector, Options, none),
    VarInstance = uppercase(proplists:get_value(variable_instance, Options, none)),
    CompInstance = uppercase(proplists:get_value(component_instance, Options, none)),
    CompName = uppercase(ComponentName),
    VarName = uppercase(VariableName),
    maybe
        [] ?= ets:match(Characteristics, {{{CompName, EVSE, Connector, CompInstance}, {VarName, VarInstance}}, '$1'}),
        [] ?= ets:match(Characteristics, {{{CompName, EVSE, Connector, CompInstance}, {VarName, any}}, '$1'}),

        [] ?= ets:match(Characteristics, {{{CompName, EVSE, Connector, any}, {VarName, VarInstance}}, '$1'}),
        [] ?= ets:match(Characteristics, {{{CompName, EVSE, Connector, any}, {VarName, any}}, '$1'}),

        [] ?= ets:match(Characteristics, {{{CompName, EVSE, any, CompInstance}, {VarName, VarInstance}}, '$1'}),
        [] ?= ets:match(Characteristics, {{{CompName, EVSE, any, CompInstance}, {VarName, any}}, '$1'}),

        [] ?= ets:match(Characteristics, {{{CompName, EVSE, any, any}, {VarName, VarInstance}}, '$1'}),
        [] ?= ets:match(Characteristics, {{{CompName, EVSE, any, any}, {VarName, any}}, '$1'}),

        [] ?= ets:match(Characteristics, {{{CompName, any, Connector, CompInstance}, {VarName, VarInstance}}, '$1'}),
        [] ?= ets:match(Characteristics, {{{CompName, any, Connector, CompInstance}, {VarName, any}}, '$1'}),

        [] ?= ets:match(Characteristics, {{{CompName, any, Connector, any}, {VarName, VarInstance}}, '$1'}),
        [] ?= ets:match(Characteristics, {{{CompName, any, Connector, any}, {VarName, any}}, '$1'}),

        [] ?= ets:match(Characteristics, {{{CompName, any, any, CompInstance}, {VarName, VarInstance}}, '$1'}),
        [] ?= ets:match(Characteristics, {{{CompName, any, any, CompInstance}, {VarName, any}}, '$1'}),

        [] ?= ets:match(Characteristics, {{{CompName, any, any, any}, {VarName, VarInstance}}, '$1'}),
        [] ?= ets:match(Characteristics, {{{CompName, any, any, any}, {VarName, any}}, '$1'}),

        [] ?= ets:match(Characteristics, {{{any, any, any, CompInstance}, {VarName, VarInstance}}, '$1'}),
        [] ?= ets:match(Characteristics, {{{any, any, any, CompInstance}, {VarName, any}}, '$1'}),

        {error, undefined}
    else
        Matches when length(Matches) > 1 -> {error, ambiguous};
        [[VarCharacteristics]] -> {ok, VarCharacteristics}
    end.


%% @doc Add a variable to the device model by specifying its
%% characteristics. If the variable already exists `false' is
%% returned, otherwise returns `true'.
-spec add_variable(DeviceModel :: device_model(),
                   ComponentName :: string(),
                   VariableName :: string(),
                   VariableIdentifiers :: [OptionalIdentifier],
                   Characteristics :: charactersitics()) ->
          boolean()
              when OptionalIdentifier :: {evse, pos_integer()} |
                                         {connector, pos_integer()} |
                                         {variable_instance, string()} |
                                         {component_instance, string()}.
add_variable(DeviceModel,
             ComponentName,
             VariableName,
             VariableIdentifiers,
             Characteristics) ->
    Component = make_component_identifier(ComponentName, VariableIdentifiers),
    Variable = make_variable_identifier(VariableName, VariableIdentifiers),
    ets:insert(DeviceModel#device_model.characteristics,
               {{Component, Variable}, Characteristics}).

-spec add_attribute(DeviceModel :: device_model(),
                    ComponentName :: string(),
                    VariableName :: string(),
                    VariableIdentifiers :: [OptionalIdentifier],
                    AttributeType :: attribute_type(),
                    AttributeValue :: binary() | undefined,
                    Mutability :: [read | write],
                    Persistent :: boolean()) ->
          boolean()
              when OptionalIdentifier :: {evse, pos_integer()} |
                                         {connector, pos_integer()} |
                                         {variable_instance, string()} |
                                         {component_instance, string()}.

update_attribute(DeviceModel,
                 ComponentName,
                 VariableName,
                 VariableIdentifiers,
                 AttributeType,
                 AttributeValue) ->
    Type = get_type(DeviceModel, ComponentName, VariableName, VariableIdentifiers),
    Value = parse_value(Type, AttributeValue),
    Component = make_component_identifier(ComponentName, VariableIdentifiers),
    Variable = make_variable_identifier(VariableName, VariableIdentifiers),
    case ets:lookup(DeviceModel#device_model.attributes, {Component, Variable, AttributeType}) of
        [] ->
            add_attribute(DeviceModel, ComponentName, VariableName, VariableIdentifiers, AttributeType, AttributeValue);
        [_] ->
            ets:update_element(DeviceModel#device_model.attributes, {Component, Variable, AttributeType}, [{2, Value}])
    end.

%% Add an attribute with default mutability and persistence values
add_attribute(DeviceModel, ComponentName, VariableName, VariableIdentifiers, AttributeType, AttributeValue) ->
    add_attribute(
      DeviceModel,
      ComponentName,
      VariableName,
      VariableIdentifiers,
      AttributeType,
      AttributeValue,
      [read, write],
      false).

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
    Component = make_component_identifier(ComponentName, VariableIdentifiers),
    Variable = make_variable_identifier(VariableName, VariableIdentifiers),
    ets:insert(DeviceModel#device_model.attributes,
               {{Component, Variable, AttributeType}, Value, Mutability, Persistent}).

make_component_identifier(Name, Options) ->
    {uppercase(Name),
     proplists:get_value(evse, Options, none),
     proplists:get_value(connector, Options, none),
     uppercase(proplists:get_value(component_instance, Options, none))}.

make_variable_identifier(Name, Options) ->
    {uppercase(Name), uppercase(proplists:get_value(variable_instance, Options, none))}.

get_type(DeviceModel, ComponentName, VariableName, Options) ->
    case characteristics(DeviceModel, ComponentName, VariableName, Options) of
        {ok, Characteristics} ->
            maps:get(datatype, Characteristics);
        {error, Reason} -> error(Reason)
    end.

%% @doc Return the value of the attribute of the given variable. If
%% the variable or attribute is undefined the call fails with reason `undefined'.
%% If the specified variable is ambiguous the call fails with reason `ambiguous'.
-spec get_value(DeviceModel :: device_model(),
                ComponentName :: string(),
                VariableName :: string(),
                VariableIdentifiers :: [OptionalIdentifier],
                AttributeType :: attribute_type()) ->
          any()
              when OptionalIdentifier :: {evse, pos_integer()} |
                                         {connector, pos_integer()} |
                                         {variable_instance, string()} |
                                         {component_instance, string()}.
get_value(#device_model{attributes = Attributes},
          ComponentName,
          VariableName,
          VariableIdentifiers,
          AttributeType) ->
    Component = make_component_identifier(ComponentName, VariableIdentifiers),
    Variable = make_variable_identifier(VariableName, VariableIdentifiers),
    case ets:match(Attributes,
                   {{Component,
                     Variable,
                     AttributeType},
                    '$1', '_', '_'})
    of
        [[Value]] -> Value;
        [] -> error(undefined);
        _ -> error(ambiguous)
    end.

parse_value(_, undefined) ->
    undefined;
parse_value(_, <<"">>) ->
    undefined;
parse_value(string, Value) ->
    Value;
parse_value(decimal, Value) ->
    binary_to_float(Value);
parse_value(integer, Value) ->
    binary_to_integer(Value);
parse_value(dateTime, Value) ->
    calendar:rfc3339_to_system_time(Value);
parse_value(boolean, <<"true">>) ->
    true;
parse_value(boolean, <<"false">>) ->
    false;
%% TODO parse OptionList/MemberList/SequenceList into lists of strings
parse_value(_, Value) ->
    Value.

default_variable_characteristics() ->
    VarDefs =
        application:get_env(ocpp, variable_defs_path,
                            filename:join(code:priv_dir(ocpp), "default_variables")),
    {ok, Defs} = file:consult(VarDefs),
    Defs.
