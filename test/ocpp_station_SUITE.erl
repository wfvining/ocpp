-module(ocpp_station_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-compile(export_all).

-define(stationid(Case),
        list_to_binary(
          atom_to_list(?MODULE) ++ "_" ++ atom_to_list(Case) ++ "_station")).

all() ->
    [connect_station,
     {group, start},
     {group, handler}].

groups() ->
    [{handler, [init_error, {group, boot_request}]},
     {boot_request, [boot_accepted, boot_rejected, boot_pending]},
     {start, [start_after_stop, start_twice]}].

init_per_suite(Config) ->
    application:ensure_all_started(jerk),
    %% Load the schemas.
    PrivDir = code:priv_dir(ocpp),
    ok = jerk:load_schema(
           filename:join(
             [PrivDir, "json_schemas", "BootNotificationRequest.json"])),
    ok = jerk:load_schema(
           filename:join(
             [PrivDir, "json_schemas", "BootNotificationResponse.json"])),
    Config.

end_per_suite(Config) ->
    application:stop(jerk),
    Config.

init_per_testcase(init_error = Case, Config) ->
    {ok, Apps} = application:ensure_all_started(gproc),
    StationId = ?stationid(Case),
    {ok, Sup} = ocpp_station_supersup:start_link(),
    [{stationid, StationId},
     {apps, Apps},
     {supersup, Sup} | Config];
init_per_testcase(Case, Config) ->
    {ok, Apps} = application:ensure_all_started(gproc),
    StationId = ?stationid(Case),
    {ok, Sup} = ocpp_station_supersup:start_link(),
    {ok, StationSup} =
        ocpp_station_supersup:start_station(
          StationId, 2, {testing_handler, nil}),
    [{stationid, StationId},
     {ok, Apps},
     {stationsup, StationSup},
     {supersup, Sup} | Config].

end_per_testcase(Config) ->
    SuperSup = ?config(supersup, Config),
    gen_server:stop(SuperSup),
    [application:stop(App) || App <- ?config(apps, Config)],
    Config.

connect_station() ->
    [{doc, "Can only connect to a station once."}].
connect_station(Config) ->
    StationId = ?config(stationid, Config),
    ok = ocpp_station:connect(StationId),
    {error, already_connected} = ocpp_station:connect(StationId).

start_twice() ->
    [{doc, "Can't start a station that is already running"}].
start_twice(Config) ->
    StationId = ?config(stationid, Config),
    ManagerPid = ocpp_station_manager:whereis(StationId),
    {error, {already_started, ManagerPid}} =
        ocpp_station_supersup:start_station(StationId, 2, {testing_handler, nil}).

start_after_stop() ->
    [{doc, "Can start and connect to a station with the "
      "same name as one that has been stopped."}].
start_after_stop(Config) ->
    StationId = ?config(stationid, Config),
    StationSup = ?config(stationsup, Config),
    ok = ocpp_station:connect(StationId),
    gen_server:stop(StationSup),
    %% Give a moment for everything to die.
    timer:sleep(100),
    {ok, _NewStationSup} =
        ocpp_station_supersup:start_station(
          StationId, 2, {testing_handler, nil}),
    ok = ocpp_station:connect(StationId).

init_error() ->
    [{doc, "The station is not started if there is an error from "
      "the handler callback module's init/1 function."}].
init_error(Config) ->
    StationId = ?config(stationid, Config),
    {error, {handler, {init, 'init error test'}}} =
        ocpp_station_supersup:start_station(
          StationId, 1, {testing_handler, {error, 'init error test'}}),
    ?assertExit({noproc, _}, ocpp_station:connect(StationId)),
    {ok,_} = ocpp_station_supersup:start_station(
               StationId, 1, {testing_handler, nil}),
    ok = ocpp_station:connect(StationId).

boot_accepted() ->
    [{doc,
      "A boot request received by the station is accepted by the handler"},
     {timetrap, 10000}].
boot_accepted(Config) ->
    StationId = ?config(stationid, Config),
    ok = ocpp_station:connect(StationId),
    TimeStr = list_to_binary(
                calendar:system_time_to_rfc3339(12345678, [{offset, "Z"}])),
    Interval = 1234,
    Payload =
        #{"chargingStation" =>
              #{"model" => <<"ct_model">>,
                "vendorName" =>  <<"foo">>},
          "reason" => <<"PowerUp">>,
          "customData" =>
              #{"testAction" => <<"ACCEPT">>,
                "interval" => Interval,
                "currentTime" => TimeStr,
                "vendorId" => <<"foo">>}},
    Req = ocpp_message:new(<<"BootNotificationRequest">>, Payload),
    {ok, Response} = ocpp_station:rpc(StationId, Req),
    ?assertEqual(<<"Accepted">>, ocpp_message:get(<<"status">>, Response)),
    ?assertEqual(Interval, ocpp_message:get(<<"interval">>, Response)),
    ?assertEqual(ocpp_message:id(Req), ocpp_message:id(Response)),
    ?assertEqual(TimeStr, ocpp_message:get(<<"currentTime">>, Response)).

boot_pending(_Config) ->
    ?assert(false).

boot_rejected(_Config) ->
    ?assert(false).
