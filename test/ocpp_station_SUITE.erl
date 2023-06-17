-module(ocpp_station_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-compile(export_all).

-define(stationid(Case),
        list_to_binary(
          atom_to_list(?MODULE) ++ "_" ++ atom_to_list(Case) ++ "_station")).

all() ->
    [connect_station,
     %% action_not_supported,
     {group, start},
     {group, disconnect},
     {group, handler}].

groups() ->
    [{handler, [init_error, {group, boot_request}]},
     {boot_request, [boot_accepted, boot_rejected, boot_pending,
                     {group, boot_error}]},
     {boot_error, [handler_error_return, handler_exit, handler_error]},
     {start, [start_after_stop, start_twice]},
     {disconnect, [{group, disconnect_after},
                   disconnect_before_request,
                   disconnect_during_request]},
     {disconnect_after, [disconnect_after_accept,
                         disconnect_after_pending,
                         disconnect_after_reject]}].

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
    timer:sleep(100),
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
    [{doc, "A response with status \"Accepted\" is received "
           "when the handler sets boot status to accepted."},
     {timetrap, 5000}].
boot_accepted(Config) ->
    StationId = ?config(stationid, Config),
    do_boot_test(accept, StationId).

boot_pending() ->
    [{doc, "A response with status \"Pending\" is received "
           "when the handler sets boot status to pending."},
     {timetrap, 5000}].
boot_pending(Config) ->
    StationId = ?config(stationid, Config),
    do_boot_test(pending, StationId).

boot_rejected() ->
    [{doc, "A response with status \"Rejected\" is received "
           "when the handler sets boot status to rejected."},
     {timetrap, 5000}].
boot_rejected(Config) ->
    StationId = ?config(stationid, Config),
    do_boot_test(rejected, StationId).

do_boot_test(accept, StationId) ->
    do_boot_test(<<"ACCEPT">>, <<"Accepted">>, StationId);
do_boot_test(pending, StationId) ->
    do_boot_test(<<"PENDING">>, <<"Pending">>, StationId);
do_boot_test(rejected, StationId) ->
    do_boot_test(<<"REJECT">>, <<"Rejected">>, StationId).

do_boot_test(Action, ExpectedStatus, StationId) ->
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
              #{"testAction" => Action,
                "interval" => Interval,
                "currentTime" => TimeStr,
                "vendorId" => <<"foo">>}},
    Req = ocpp_message:new(<<"BootNotificationRequest">>, Payload),
    {ok, Response} = ocpp_station:rpc(StationId, Req),
    ?assertEqual(ExpectedStatus, ocpp_message:get(<<"status">>, Response)),
    ?assertEqual(Interval, ocpp_message:get(<<"interval">>, Response)),
    ?assertEqual(ocpp_message:id(Req), ocpp_message:id(Response)),
    ?assertEqual(TimeStr, ocpp_message:get(<<"currentTime">>, Response)).

handler_error_return() ->
    [{doc, "When the handler returns an error it is returned "
           "to the caller with the error information it provided."},
     {timetrap, 5000}].
handler_error_return(Config) ->
    StationId = ?config(stationid, Config),
    ok = ocpp_station:connect(StationId),
    Payload =
        #{"chargingStation" =>
              #{"model" => <<"ct_model">>,
                "vendorName" =>  <<"foo">>},
          "reason" => <<"PowerUp">>,
          "customData" =>
              #{"testAction" => <<"ERROR">>,
                "errorReason" => <<"because error">>,
                "vendorId" => <<"handle_error_return">>}},
    Req = ocpp_message:new(<<"BootNotificationRequest">>, Payload),
    {error, Error} = ocpp_station:rpc(StationId, Req),
    %% There are 4 components to an OCPP error:
    %% 1. MessageId
    %% 2. ErrorCode (NotSupported, InternalError...)
    %% 3. ErrorDescription an informative string (also from the table?)
    %% 4. ErrorDetails a JSON object.
    ?assertEqual(ocpp_message:id(Req), ocpp_error:id(Error)),
    ?assertEqual(<<"InternalError">>, ocpp_error:code(Error)),
    ?assertEqual(<<"An internal error occurred and the receiver "
                   "was not able to process the requested Action "
                   "successfully">>,
                 ocpp_error:description(Error)),
    ?assertEqual(#{<<"reason">> => <<"because error">>},
                 ocpp_error:details(Error)),
    test_new_boot_request_works(StationId, Payload).

handler_error() ->
    [{doc, "When the handler fails by raising an error "
           "an 'InternalError' is returned to the requestor."},
     {timetrap, 5000}].
handler_error(Config) ->
    StationId = ?config(stationid, Config),
    ok = ocpp_station:connect(StationId),
    Payload =
        #{"chargingStation" =>
              #{"model" => <<"ct_model">>,
                "vendorName" =>  <<"foo">>},
          "reason" => <<"PowerUp">>,
          "customData" =>
              #{"testAction" => <<"CRASH">>,
                "errorReason" => <<"because error/1">>,
                "vendorId" => <<"handle_error">>}},
    Req = ocpp_message:new(<<"BootNotificationRequest">>, Payload),
    {error, Error} = ocpp_station:rpc(StationId, Req),
    ?assertEqual(ocpp_message:id(Req), ocpp_error:id(Error)),
    ?assertEqual(<<"InternalError">>, ocpp_error:code(Error)),
    ?assertEqual(<<"An internal error occurred and the receiver "
                   "was not able to process the requested Action "
                   "successfully">>,
                 ocpp_error:description(Error)),
    ?assertEqual(#{<<"reason">> => <<"because error/1">>},
                 ocpp_error:details(Error)),
    test_new_boot_request_works(StationId, Payload).

handler_exit() ->
    [{doc, "When the handler exits the an 'InternalError' is "
           "returned to the requestor."},
     {timetrap, 5000}].
handler_exit(Config) ->
    StationId = ?config(stationid, Config),
    ok = ocpp_station:connect(StationId),
    Payload =
        #{"chargingStation" =>
              #{"model" => <<"ct_model">>,
                "vendorName" =>  <<"foo">>},
          "reason" => <<"PowerUp">>,
          "customData" =>
              #{"testAction" => <<"EXIT">>,
                "errorReason" => <<"because exit/1">>,
                "vendorId" => <<"handle_exit">>}},
    Req = ocpp_message:new(<<"BootNotificationRequest">>, Payload),
    {error, Error} = ocpp_station:rpc(StationId, Req),
        ?assertEqual(ocpp_message:id(Req), ocpp_error:id(Error)),
    ?assertEqual(<<"InternalError">>, ocpp_error:code(Error)),
    ?assertEqual(<<"An internal error occurred and the receiver "
                   "was not able to process the requested Action "
                   "successfully">>,
                 ocpp_error:description(Error)),
    ?assertEqual(#{<<"reason">> => <<"because exit/1">>}, ocpp_error:details(Error)),
    test_new_boot_request_works(StationId, Payload).

test_new_boot_request_works(StationId, Payload) ->
    NewReq = ocpp_message:new(
               <<"BootNotificationRequest">>,
               Payload#{"customData" =>
                            #{"testAction" => <<"ACCEPT">>,
                              "interval" => 1,
                              "currentTime" => <<"2023-06-15T15:30.00Z">>,
                              "vendorId" => <<"handle_error">>}}),
    ct:log("trying new message..."),
    {ok, Response} = ocpp_station:rpc(StationId, NewReq),
    ?assertEqual(<<"Accepted">>, ocpp_message:get(<<"status">>, Response)).

disconnect_after_accept() ->
    [{doc, "After being accepted the station disconnects and a "
           "different process can connect."},
     {timetrap, 5000}].
disconnect_after_accept(Config) ->
    Station = ?config(stationid, Config),
    disconnect_after_x(Station, <<"ACCEPT">>).

disconnect_after_reject() ->
    [{timetrap, 5000}].
disconnect_after_reject(Config) ->
    Station = ?config(stationid, Config),
    disconnect_after_x(Station, <<"REJECT">>).

disconnect_after_pending() ->
    [{timetrap, 5000}].
disconnect_after_pending(Config) ->
    Station = ?config(stationid, Config),
    disconnect_after_x(Station, <<"PENDING">>).

disconnect_after_x(Station, TestAction) ->
    F = fun () ->
                ok = ocpp_station:connect(Station),
                {ok, Response} =
                    ocpp_station:rpc(
                      Station,
                      ocpp_message:new(
                        <<"BootNotificationRequest">>,
                        #{"chargingStation" =>
                              #{"model" => <<"ct_model">>,
                                "vendorName" =>  <<"foo">>},
                          "reason" => <<"PowerUp">>,
                          "customData" =>
                              #{"testAction" => TestAction,
                                "interval" => 1,
                                "currentTime" =>
                                    list_to_binary(
                                      calendar:system_time_to_rfc3339(
                                        12341234,
                                        [{offset, "Z"}])),
                                "vendorId" => <<"disconnect_after_x">>}})),
                ?assertEqual(
                   action_to_status(TestAction),
                   ocpp_message:get(<<"status">>, Response))
        end,
    {Pid, Ref} = spawn_monitor(F),
    receive
        {'DOWN', Ref, process, Pid, normal} ->
            ok = ocpp_station:connect(Station);
        {'DOWN', Ref, process, Pid, Reason} ->
            ct:log("Initial connected process failed: ~p", Reason),
            ct:abort_current_testcase(Reason)
    end.

action_to_status(<<"ACCEPT">>)  -> <<"Accepted">>;
action_to_status(<<"REJECT">>)  -> <<"Rejected">>;
action_to_status(<<"PENDING">>) -> <<"Pending">>.

disconnect_before_request() ->
    [{doc, "Before making a request the station disconnects and a "
           "different process can connect."},
     {timetrap, 5000}].
disconnect_before_request(Config) ->
    Station = ?config(stationid, Config),
    F = fun () ->
                ok = ocpp_station:connect(Station)
        end,
    {Pid, Ref} = spawn_monitor(F),
    receive
        {'DOWN', Ref, process, Pid, normal} ->
            ok = ocpp_station:connect(Station);
        {'DOWN', Ref, process, Pid, Reason} ->
            ct:log("Initial connected process failed: ~p", Reason),
            ct:abort_current_testcase(Reason)
    end.

disconnect_during_request() ->
    [{doc, "If the station disconnects and reconnects while its previous "
           "request is still being processed, the CSMS response to the "
           "previous request is dropped and only the response to a new request "
           "is received by the station."},
     {timetrap, 10000}].
disconnect_during_request(Config) ->
    Station = ?config(stationid, Config),
    CustomData = #{"testAction" => <<"REJECT">>,
                   "interval" => 1,
                   "currentTime" =>
                       list_to_binary(
                         calendar:system_time_to_rfc3339(
                           12341234,
                           [{offset, "Z"}])),
                   "vendorId" => <<"disconnect_during_request">>},
    Req = #{"chargingStation" =>
                #{"model" => <<"ct_model">>,
                  "vendorName" =>  <<"foo">>},
            "reason" => <<"PowerUp">>,
            "customData" => CustomData#{"delay" => 1000}},
    F = fun() ->
                ok = ocpp_station:connect(Station),
                {ok, _Response} =
                    ocpp_station:rpc(
                      Station, ocpp_message:new(<<"BootNotificationRequest">>, Req))
        end,
    {Pid, Ref} = spawn_monitor(F),
    timer:sleep(100),
    exit(Pid, 'time to die'),
    receive
        {'DOWN', Ref, process, Pid, 'time to die'} ->
            %% Do new connection and request
            ok = ocpp_station:connect(Station),
            Msg = ocpp_message:new(
                    <<"BootNotificationRequest">>,
                    Req#{"customData" =>
                             CustomData#{"testAction" => <<"ACCEPT">>}}),
            {ok, Response} = ocpp_station:rpc(Station, Msg),
            ?assertEqual(<<"Accepted">>, ocpp_message:get(<<"status">>, Response));
        _ ->
            ct:log("Unnexepcted message received in test case"),
            ct:fail('unnexpected message')
    end.
