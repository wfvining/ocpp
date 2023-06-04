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
    [{handler, [init_error]}, %% TODO add tests for handling OCPP requests
     {start, [start_after_stop, start_twice]}].

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
          StationId, 2, {do_nothing_handler, nil}),
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
        ocpp_station_supersup:start_station(StationId, 2, {do_nothing_handler, nil}).

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
          StationId, 2, {do_nothing_handler, nil}),
    ok = ocpp_station:connect(StationId).

init_error() ->
    [{doc, "The station is not started if there is an error from "
      "the handler callback module's init/1 function."}].
init_error(Config) ->
    StationId = ?config(stationid, Config),
    {error, {handler, {init, 'init error test'}}} =
        ocpp_station_supersup:start_station(
          StationId, 1, {do_nothing_handler, {error, 'init error test'}}),
    ?assertExit({noproc, _}, ocpp_station:connect(StationId)).
