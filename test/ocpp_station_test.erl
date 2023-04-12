-module(ocpp_station_test).

-include_lib("eunit/include/eunit.hrl").

-define(STATION_ID, <<"TestStation">>).

start_test_() ->
    {"When a station starts its id should be registered with the station registry",
     {setup,
      fun start_station/0,
      fun stop_station/1,
      fun station_registered/1}}.

stop_test_() ->
    {"When a station stops its id should be unregisterd.",
     {foreach,
      fun start_station_trap_exit/0,
      fun stop_station/1,
      [fun normal_shutdown_unregister/1,
       fun abnormal_shutdown_unregister/1]}}.

duplicate_id_test_() ->
    {"It should be impossible to start a station with the same id as another",
     {setup,
      fun start_station/0,
      fun stop_station/1,
      fun test_duplicate_station_id/1}}.

%% setup/teardown
start_station_trap_exit() ->
    process_flag(trap_exit, true),
    start_station().

start_station() ->
    {ok, _StationRegistry} = ocpp_station_registry:start_link(),
    {ok, StationPid} = ocpp_station:start_link(?STATION_ID),
    StationPid = ocpp_station_registry:whereis_name(?STATION_ID),
    #{stationid => ?STATION_ID, station => StationPid}.

stop_station(#{station := StationPid}) ->
    exit(StationPid, normal),
    ocpp_station_registry:stop(),
    %% Pause to ensure everything gets shut down before starting the
    %% next test
    timer:sleep(10).

station_registered(#{stationid := StationId,
                     station := StationPid}) ->
    Pid = ocpp_station_registry:whereis_name(StationId),
    ?_assertEqual(StationPid, Pid).

normal_shutdown_unregister(#{stationid := StationId}) ->
    ok = ocpp_station:stop(StationId),
    assert_unregistered(StationId).

abnormal_shutdown_unregister(#{station := StationPid,
                               stationid := StationId}) ->
    exit(StationPid, kill),
    [?_assertNot(is_process_alive(StationPid)),
     assert_unregistered(StationId)].

assert_unregistered(StationId) ->
    ?_assertEqual(
       undefined,
       ocpp_station_registry:whereis_name(StationId)).

test_duplicate_station_id(_State) ->
    ?_assertMatch(
       {error, {already_started, _}},
       ocpp_station:start_link(?STATION_ID)).
