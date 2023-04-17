-module(ocpp_station_test).

-include_lib("eunit/include/eunit.hrl").

-define(STATION_ID, <<"TestStation">>).
-define(NUM_EVSE, 2).

start_test_() ->
    {"When a station starts its id should be registered with the station registry",
     {setup,
      fun start_station/0,
      fun stop_station/1,
      fun station_registered/1}}.

stop_test_() ->
    {"When a station stops its id should be unregisterd.",
     {setup,
      fun start_station/0,
      fun stop_station/1,
      fun normal_shutdown_unregister/1}}.

duplicate_id_test_() ->
    {"It should be impossible to start a station with the same id as another",
     {setup,
      fun start_station/0,
      fun stop_station/1,
      fun test_duplicate_station_id/1}}.

connect_test_() ->
    {"It should be possible for a single process to connect to a station.",
     {setup,
      fun start_station/0,
      fun stop_station/1,
      fun test_station_connect/1}}.

reconnect_test_() ->
    {"After a connection dies a new process can connect to a station.",
     {setup,
      fun start_station/0,
      fun stop_station/1,
      fun test_station_reconnect/1}}.

%% setup/teardown
start_station() ->
    ocpp_station_registry:new(),
    {ok, StationSup} = ocpp_station_sup:start_link(?STATION_ID, ?NUM_EVSE),
    #{stationid => ?STATION_ID, station_sup => StationSup}.

stop_station(#{station_sup := StationSup}) ->
    process_flag(trap_exit, true),
    ocpp_station_sup:stop(StationSup),
    timer:sleep(100),
    ocpp_station_registry:delete(),
    timer:sleep(100).

station_registered(#{stationid := StationId}) ->
    Pid = ocpp_station:lookup(StationId),
    ?_assert(is_pid(Pid)).

normal_shutdown_unregister(#{stationid := StationId}) ->
    Station = ocpp_station:lookup(StationId),
    ok = ocpp_station:stop(Station),
    timer:sleep(100),
    NewStation = ocpp_station:lookup(StationId),
    ?_assertNotEqual(Station, NewStation).

test_duplicate_station_id(#{stationid := StationId}) ->
    fun() ->
            process_flag(trap_exit, true),
            StationPid = ocpp_station:lookup(StationId),
            ?assertMatch({error, {already_registered, StationPid}},
                         ocpp_station:start_link(?STATION_ID, ?NUM_EVSE))
    end.

test_station_connect(#{stationid := StationId}) ->
    StationPid = ocpp_station:lookup(StationId),
    {inorder,
     [?_assertEqual(ok, ocpp_station:connect(StationPid, self())),
      {spawn, ?_assertEqual({error, already_connected},
                            ocpp_station:connect(StationPid, self()))}]}.

test_station_reconnect(#{stationid := StationId}) ->
    StationPid = ocpp_station:lookup(StationId),
    {Pid, Ref}  = spawn_monitor(
                    fun() ->
                            ok = ocpp_station:connect(StationPid, self())
                    end),
    %% Wait for the first process to exit.
    receive {'DOWN', Ref, process, Pid, _} -> ok end,
    ?_assertEqual(ok, ocpp_station:connect(StationPid, self())).
