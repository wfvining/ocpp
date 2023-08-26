-module(ocpp_message_test).

-include_lib("eunit/include/eunit.hrl").

-define(config(Key, Config), element(2, lists:keyfind(Key, 1, Config))).

setup_apps() ->
    {ok, Apps} = application:ensure_all_started(jerk),
    [{apps, Apps}].

teardown_apps(Config) ->
    [ application:stop(App) || App <- ?config(apps, Config)].

load_schemas() ->
    Config = setup_apps(),
    PrivDir = code:priv_dir(ocpp),
    {ok, Files} = file:list_dir(filename:join(PrivDir, "json_schemas")),
    SchemaFiles = lists:filter(
                    fun(FileName) ->
                            filename:extension(FileName) =:= ".json"
                    end, Files),
    [ok = jerk:load_schema(filename:join([PrivDir, "json_schemas", File]))
     || File <- SchemaFiles],
    Config.

create_message_test_() ->
    {setup, fun load_schemas/0, fun teardown_apps/1,
     {inparallel,
      [ fun construct_from_map/0
      , construct_with_error()
      ]}}.

get_default_test_() ->
    {"Can provide a default to `get/3' which will be returned if the "
     "requested key is not present.",
     {setup, fun load_schemas/0, fun teardown_apps/1,
      {inparallel,
       [?_test(
           ?assertEqual(nil,
                        ocpp_message:get(
                          <<"foo">>,
                          ocpp_message:new_request(
                            'CancelReservation', #{<<"reservationId">> => 123}),
                          nil))),
        ?_test(
           ?assertEqual(nil,
                        ocpp_message:get(
                          <<"foo/bar">>,
                          ocpp_message:new_request(
                            'CancelReservation', #{<<"reservationId">> => 123}),
                          nil))),
        ?_test(
           ?assertError(_,
                        ocpp_message:get(
                          <<"reservationId/foo">>,
                          ocpp_message:new_request(
                            'CancelReservation', #{<<"reservationId">> => 123}),
                          nil))),
        ?_test(
           ?assertError({undefined, _},
                        ocpp_message:get(
                          <<"foo">>,
                          ocpp_message:new_request(
                            'CancelReservation', #{<<"reservationId">> => 123}))))]}}}.

get_nested_test_() ->
    {"Can separate a nested object from its message but still use the"
     "ocpp_message API to access its fields.",
     {setup, fun load_schemas/0, fun teardown_apps/1,
      [fun() ->
               MessageType = 'CancelReservation',
               Payload = #{<<"reservationId">> => 123,
                           <<"customData">> =>
                               #{<<"foo">> => [1, 2, 3],
                                 <<"vendorId">> => <<"this is the vendor id">>}},
               Message = ocpp_message:new_request(MessageType, Payload),
               CustomData = ocpp_message:get(<<"customData">>, Message),
               ?assertEqual([1, 2, 3], ocpp_message:get(<<"foo">>, CustomData))
       end]}}.

get_message_id_test_() ->
    {"ocpp_message:id/1 returns the message id",
     {setup, fun load_schemas/0, fun teardown_apps/1,
      [fun() ->
               MessageType = 'CancelReservation',
               Payload = #{<<"reservationId">> => 123,
                           <<"customData">> =>
                               #{<<"foo">> => [1, 2, 3],
                                 <<"vendorId">> => <<"this is the vendor id">>}},
               MessageId = <<"abcdefg">>,
               Message = ocpp_message:new_request(MessageType, Payload, MessageId),
               CustomData = ocpp_message:get(<<"customData">>, Message),
               ?assertEqual(<<"abcdefg">>, ocpp_message:id(Message)),
               ?assertEqual(<<"abcdefg#/customData">>, ocpp_message:id(CustomData))
       end]}}.

construct_from_map() ->
    MessageType = 'CancelReservation',
    Payload = #{<<"reservationId">> => 123,
                <<"customData">> =>
                    #{<<"foo">> => [1, 2, 3],
                      <<"vendorId">> => <<"this is the vendor id">>}},
    Message = ocpp_message:new_request(MessageType, Payload),
    ?assertEqual([<<"customData">>, <<"reservationId">>],
                 ocpp_message:properties(Message)),
    ?assertEqual(123, ocpp_message:get(<<"reservationId">>, Message)),
    ?assertEqual(<<"this is the vendor id">>,
                 ocpp_message:get(<<"customData/vendorId">>, Message)).

construct_with_error() ->
    {"constructing a message with an invalid property name or value "
     "causes a badarg error",
     {inparallel,
      [?_assertError(
          badarg,
          ocpp_message:new_request('CancelReservation', Message))
       || Message <- [#{<<"bad-key">> => 2323, <<"reservationId">> => 123},
                      #{<<"reservationId">> => <<"not-an-integer">>},
                      #{}]]}}.

type_test_() ->
    {"ocpp_message:type/1 returns the name of the schema.",
     {setup, fun load_schemas/0, fun teardown_apps/1,
      fun () ->
              Message =
                  ocpp_message:new_request(
                    'CancelReservation',
                    #{<<"reservationId">> => 123,
                      <<"customData">> => #{<<"vendorId">> => <<"foo">>}}),
              ?assertEqual(<<"CancelReservationRequest">>,
                           ocpp_message:type(Message))
              %% This is broken in jerk @ 798d231
              %% ?assertEqual(<<"CancelReservationRequest#/customData">>,
              %%              ocpp_message:type(Message))
     end}}.

atom_keys_test_() ->
    {"can construct a message from a map that uses atoms for keys.",
     {setup,
      fun load_schemas/0,
      fun teardown_apps/1,
      fun() ->
              Message =
                  ocpp_message:new_request('BootNotification',
                                           #{reason => 'ApplicationReset',
                                             chargingStation =>
                                                 #{model => <<"foo">>,
                                                   vendorName => <<"bar">>,
                                                   modem => #{iccid => <<"testiccid">>}},
                                             customData =>
                                                 #{<<"foo">> => 1,
                                                   bar => <<"abcdefg">>,
                                                   vendorId => <<"vendor-foo">>}}),
              ?assertEqual(<<"ApplicationReset">>, ocpp_message:get(<<"reason">>, Message)),
              ?assertEqual(<<"testiccid">>,
                           ocpp_message:get(
                             <<"iccid">>,
                             ocpp_message:get(
                               <<"modem">>,
                               ocpp_message:get(<<"chargingStation">>, Message))))
      end}}.
