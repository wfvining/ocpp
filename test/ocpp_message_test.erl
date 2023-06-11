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

get_nested_test_() ->
    {"Can separate a nested object from its message but still use the"
     "ocpp_message API to access its fields.",
     {setup, fun load_schemas/0, fun teardown_apps/1,
      [fun() ->
               MessageType = <<"CancelReservationRequest">>,
               Payload = #{<<"reservationId">> => 123,
                           <<"customData">> =>
                               #{<<"foo">> => [1, 2, 3],
                                 <<"vendorId">> => <<"this is the vendor id">>}},
               Message = ocpp_message:new(MessageType, Payload),
               CustomData = ocpp_message:get(<<"customData">>, Message),
               ?assertEqual([1, 2, 3], ocpp_message:get(<<"foo">>, CustomData))
       end]}}.

get_message_id_test_() ->
    {"ocpp_message:id/1 returns the message id",
     {setup, fun load_schemas/0, fun teardown_apps/1,
      [fun() ->
               MessageType = <<"CancelReservationRequest">>,
               Payload = #{<<"reservationId">> => 123,
                           <<"customData">> =>
                               #{<<"foo">> => [1, 2, 3],
                                 <<"vendorId">> => <<"this is the vendor id">>}},
               MessageId = <<"abcdefg">>,
               Message = ocpp_message:new(MessageType, Payload, MessageId),
               CustomData = ocpp_message:get(<<"customData">>, Message),
               ?assertEqual(<<"abcdefg">>, ocpp_message:id(Message)),
               ?assertEqual(<<"abcdefg#/customData">>, ocpp_message:id(CustomData))
       end]}}.

construct_from_map() ->
    MessageType = <<"CancelReservationRequest">>,
    Payload = #{<<"reservationId">> => 123,
                <<"customData">> =>
                    #{<<"foo">> => [1, 2, 3],
                      <<"vendorId">> => <<"this is the vendor id">>}},
    Message = ocpp_message:new(MessageType, Payload),
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
          ocpp_message:new(<<"CancelReservationRequest">>, Message))
       || Message <- [#{<<"bad-key">> => 2323, <<"reservationId">> => 123},
                      #{<<"reservationId">> => <<"not-an-integer">>},
                      #{}]]}}.
