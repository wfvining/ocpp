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

%% QUESTION should I maybe support building these out of jerk terms?

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
%% TODO get raises a badkey error when the property is not defined.

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
