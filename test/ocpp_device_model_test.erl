-module(ocpp_device_model_test).

-include_lib("eunit/include/eunit.hrl").

add_get_test_() ->
    ComponentName = <<"Foo">>,
    VariableName = <<"Bar">>,
    VariableIdentifiers = #{evse => 1},
    VariableCharacteristics = #{unit => <<"kW">>, datatype => <<"decimal">>},
    {"A variable and its attributes can be added to and"
     " retrieved from the device model",
     {setup, local, fun ocpp_device_model:new/0,
      fun(DeviceModel) ->
              {inorder,
               [?_test(
                   ?assertNot(ocpp_device_model:variable_exists(DeviceModel,
                                                                ComponentName,
                                                                VariableName,
                                                                VariableIdentifiers))),
                ?_test(
                   ?assert(ocpp_device_model:add_variable(DeviceModel,
                                                          ComponentName,
                                                          VariableName,
                                                          VariableIdentifiers,
                                                          VariableCharacteristics))),
                ?_test(
                   ?assert(ocpp_device_model:variable_exists(DeviceModel,
                                                             ComponentName,
                                                             VariableName,
                                                             VariableIdentifiers))),
                ?_test(
                   ?assert(ocpp_device_model:add_attribute(DeviceModel,
                                                           ComponentName,
                                                           VariableName,
                                                           VariableIdentifiers,
                                                           'Actual',
                                                           <<"1.2">>,
                                                           [read, write],
                                                           false))),
                ?_test(
                   ?assert(ocpp_device_model:add_attribute(DeviceModel,
                                                           ComponentName,
                                                           VariableName,
                                                           VariableIdentifiers,
                                                           'Target',
                                                           <<"1.0">>,
                                                           [read, write],
                                                           false))),
                ?_test(
                   ?assertEqual(1.2, ocpp_device_model:get_value(DeviceModel,
                                                                 ComponentName,
                                                                 VariableName,
                                                                 VariableIdentifiers,
                                                                 'Actual'))),
                ?_test(
                   ?assertEqual(1.0, ocpp_device_model:get_value(DeviceModel,
                                                                 ComponentName,
                                                                 VariableName,
                                                                 VariableIdentifiers,
                                                                 'Target')))]}
      end}}.

ambiguous_identifiers_test_() ->
    {"ambiguous variable identifiers results in an error",
     {setup, local, fun populate_device_model/0,
      fun (DeviceModel) ->
              [?_assertError(ambiguous, ocpp_device_model:variable_exists(DeviceModel, <<"Foo">>, <<"Bar">>, #{})),
               ?_assertError(ambiguous, ocpp_device_model:get_value(DeviceModel, <<"Foo">>, <<"Bar">>, #{}, 'Actual')),
               ?_assertError(ambiguous, ocpp_device_model:add_attribute(DeviceModel,
                                                                        <<"Foo">>, <<"Bar">>, #{},
                                                                        'Target', <<"3.0">>, [read], false))] end}}.

undefined_test_() ->
    {setup, local, fun populate_device_model/0,
     fun (DeviceModel) ->
             [{"accessing an undefined variable results in an error",
               [?_assertError(undefined, ocpp_device_model:get_value(DeviceModel, <<"Foo">>, <<"Baz">>, #{}, 'Actual'))]},
              {"adding an attribute to an undefined variable results in an error",
               [?_assertError(undefined, ocpp_device_model:add_attribute(DeviceModel,
                                                                         <<"Foo">>, <<"Baz">>, #{},
                                                                         'Actual', <<"1.1">>, [read], false))]}]
     end}.

populate_device_model() ->
    ComponentName = <<"Foo">>,
    VariableName = <<"Bar">>,
    VariableCharacteristics = #{unit => <<"kW">>, datatype => <<"decimal">>},
    DeviceModel = ocpp_device_model:new(),
    true = ocpp_device_model:add_variable(DeviceModel, ComponentName, VariableName, #{evse => 1}, VariableCharacteristics),
    true = ocpp_device_model:add_variable(DeviceModel, ComponentName, VariableName, #{evse => 2}, VariableCharacteristics),
    true = ocpp_device_model:add_attribute(DeviceModel, ComponentName, VariableName, #{evse => 1}, 'Actual', <<"1.0">>, [read], false),
    true = ocpp_device_model:add_attribute(DeviceModel, ComponentName, VariableName, #{evse => 2}, 'Actual', <<"2.0">>, [read], false),
    DeviceModel.
