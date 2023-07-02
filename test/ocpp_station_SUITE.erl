-module(ocpp_station_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-compile(export_all).

-define(stationid(Case),
        list_to_binary(
          atom_to_list(?MODULE) ++ "_" ++ atom_to_list(Case) ++ "_station")).

-define(nowutc,
        list_to_binary(calendar:system_time_to_rfc3339(
                         erlang:system_time(second),
                         [{offset, "Z"}, {unit, second}]))).

-define(BOOT(Action),
        ocpp_message:new_request(
          'BootNotification',
          #{"chargingStation" =>
                #{"model" => <<"ct_model">>,
                  "vendorName" =>  <<"foo">>},
            "reason" => <<"PowerUp">>,
            "customData" =>
                #{"testAction" => Action,
                  "interval" => 1,
                  "currentTime" => <<"2023-06-15T15:30.00Z">>,
                  "vendorId" => <<"doboot">>}})).
-define(BOOT_ACCEPT, ?BOOT(<<"ACCEPT">>)).
-define(BOOT_REJECT, ?BOOT(<<"REJECT">>)).
-define(BOOT_PENDING, ?BOOT(<<"PENDING">>)).

all() ->
    [connect_station,
     not_supported_error,
     message_before_boot_request,
     {group, start},
     {group, disconnect},
     {group, handler},
     {group, provisioning},
     {group, offline}].

groups() ->
    [{handler, [{group, boot_request}]},
     {boot_request, [boot_accepted, boot_rejected, boot_pending,
                     {group, boot_error}]},
     {boot_error, [handler_error_return, handler_exit, handler_error]},
     {start, [handler_init_error, start_after_stop, start_twice]},
     {disconnect, [{group, disconnect_after},
                   disconnect_before_request,
                   disconnect_during_request]},
     {disconnect_after, [disconnect_after_accept,
                         disconnect_after_pending,
                         disconnect_after_reject]},
     {provisioning, [set_connector_status,
                     {group, provisioning_errors}]},
     {provisioning_errors, [provision_invalid_evse, provision_invalid_connector,
                            {group, boot_pending_messages}]},
     {boot_pending_messages, [forbidden_messages, boot_request_pending]},
     {offline, [reconnect, restart]}].


load_schemas() ->
    PrivDir = code:priv_dir(ocpp),
    SchemaDir = filename:join(PrivDir, "json_schemas"),
    {ok, Files} = file:list_dir(SchemaDir),
    [jerk:load_schema(filename:join(SchemaDir, SchemaFile))
     || SchemaFile <- Files].

init_per_suite(Config) ->
    {ok, JerkApps} = application:ensure_all_started(jerk),
    {ok, GProcApps} = application:ensure_all_started(gproc),
    %% Load the schemas.
    load_schemas(),
    [{apps, JerkApps ++ GProcApps} | Config].

end_per_suite(Config) ->
    [application:stop(App) || App <- ?config(apps, Config)],
    Config.

init_per_group(start, Config) ->
    {ok, SuperSup} = ocpp_station_supersup:start_link(),
    unlink(SuperSup),
    [{supersup, SuperSup} | Config];
init_per_group(_, Config) ->
    Config.

end_per_group(start, Config) ->
    SuperSup = ?config(supersup, Config),
    gen_server:stop(SuperSup),
    Config;
end_per_group(_, Config) ->
    Config.

init_per_testcase(Case, Config)
  when Case =:= reconnect;
       Case =:= restart ->
    StationId = ?stationid(Case),
    {ok, StationSup} = ocpp_station_sup:start_link(StationId, [ocpp_evse:new(1)], {testing_handler, nil}),
    ConnPid = spawn(
                fun F() ->
                        ok = ocpp_station:connect(StationId),
                        ocpp_station:rpccall(StationId, ?BOOT_ACCEPT),
                        {ok, _} = ocpp_station:rpccall(
                                    StationId,
                                    ocpp_message:new_request(
                                      'StatusNotification',
                                      #{"timestamp" => ?nowutc,
                                        "connectorStatus" => <<"Available">>,
                                        "evseId" => 1,
                                        "connectorId" => 1})),
                        {ok, _} = ocpp_station:rpccall(
                                    StationId, ocpp_message:new_request('Heartbeat', #{})),
                        receive _ -> F() end
                end),
    %% Give `ConnPid' enough time to get connected.
    timer:sleep(100),
    [{stationid, StationId},
     {stationsup, StationSup},
     {connpid, ConnPid}| Config];
init_per_testcase(Case, Config)
  when Case =:= forbidden_messages;
       Case =:= boot_request_pending ->
    StationId = ?stationid(provisioning),
    {ok, StationSup} = ocpp_station_sup:start_link(
                         StationId,
                         [ocpp_evse:new(2), ocpp_evse:new(2)],
                         {testing_handler, nil}),
    ok = ocpp_station:connect(StationId),
    {ok, _} = ocpp_station:rpccall(StationId, ?BOOT_PENDING),
    %% Station has been accepted, but still needs to send status notifications
    %% for its connectors.
    [{stationid, StationId},
     {stationsup, StationSup},
     {connectors, [{1, 2}, {2, 2}]},
     {evse, 2} | Config];
init_per_testcase(Case, Config)
  when Case =:= set_connector_status;
       Case =:= set_connector_status_repeat;
       Case =:= provision_invalid_evse;
       Case =:= provision_invalid_connector ->
    StationId = ?stationid(provisioning),
    {ok, StationSup} = ocpp_station_sup:start_link(
                         StationId,
                         [ocpp_evse:new(2), ocpp_evse:new(2)],
                         {testing_handler, nil}),
    ok = ocpp_station:connect(StationId),
    {ok, _} = ocpp_station:rpccall(StationId, ?BOOT_ACCEPT),
    %% Station has been accepted, but still needs to send status notifications
    %% for its connectors.
    [{stationid, StationId},
     {connectors, [{1, 2}, {2, 2}]},
     {stationsup, StationSup},
     {evse, 2} | Config];
init_per_testcase(not_supported_error = Case, Config) ->
    StationId = ?stationid(Case),
    {ok, StationSup} = ocpp_station_sup:start_link(
                         StationId,
                         [ocpp_evse:new(2), ocpp_evse:new(2)],
                         {testing_handler, nil}),
    ok = ocpp_station:connect(StationId),
    {ok, _} = ocpp_station:rpccall(StationId, ?BOOT_ACCEPT),
    {ok, _} = ocpp_station:rpccall(
                StationId, ocpp_message:new_request('Heartbeat', #{})),
    [{stationid, StationId}, {stationsup, StationSup}| Config];
init_per_testcase(handler_init_error = Case, Config) ->
    StationId = ?stationid(Case),
    [{stationid, StationId} | Config];
init_per_testcase(Case, Config)
  when Case =:= start_after_stop;
       Case =:= start_twice ->
    StationId = ?stationid(Case),
    {ok, StationSup} = ocpp_station_supersup:start_station(
                         StationId, [ocpp_evse:new(1)], {testing_handler, nil}),
    [{stationsup, StationSup}, {stationid, StationId} | Config];
init_per_testcase(Case, Config) ->
    StationId = ?stationid(Case),
    {ok, StationSup} =
        ocpp_station_sup:start_link(
          StationId,
          [ocpp_evse:new(1),
           ocpp_evse:new(1),
           ocpp_evse:new(2)],
          {testing_handler, nil}),
    [{stationid, StationId},
     {stationsup, StationSup} | Config].

end_per_testcase(Case, Config)
  when Case =:= start_after_stop;
       Case =:= start_twice;
       Case =:= handler_init_error ->
    Config;
end_per_testcase(_, Config) ->
    StationSup = ?config(stationsup, Config),
    gen_server:stop(StationSup),
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
        ocpp_station_supersup:start_station(
          StationId,
          [ocpp_evse:new(2), ocpp_evse:new(2)],
          {testing_handler, nil}).

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
          StationId, [ocpp_evse:new(1)], {testing_handler, nil}),
    ok = ocpp_station:connect(StationId).

handler_init_error() ->
    [{doc, "The station is not started if there is an error from "
      "the handler callback module's init/1 function."}].
handler_init_error(Config) ->
    StationId = ?config(stationid, Config),
    {error, {handler, {init, 'init error test'}}} =
        ocpp_station_supersup:start_station(
          StationId, 1, {testing_handler, {error, 'init error test'}}),
    ?assertExit({noproc, _}, ocpp_station:connect(StationId)),
    {ok,_} = ocpp_station_supersup:start_station(
               StationId, [ocpp_evse:new(2)], {testing_handler, nil}),
    ok = ocpp_station:connect(StationId).

message_before_boot_request() ->
    [{doc, "If a message other than a boot request is sent before the station "
           " is accepted the CSMS responds with a SecurityError."},
     {timetrap, 5000}].
message_before_boot_request(Config) ->
    StationId = ?config(stationid, Config),
    ok = ocpp_station:connect(StationId),
    Message = ocpp_message:new_request('Heartbeat', #{}),
    {error, Error} = ocpp_station:rpccall(
                       StationId,
                       Message),
    ?assertEqual(<<"SecurityError">>, ocpp_error:code(Error)),
    ?assertEqual(ocpp_message:id(Message), ocpp_error:id(Error)).

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
    Req = ocpp_message:new_request('BootNotification', Payload),
    {ok, Response} = ocpp_station:rpccall(StationId, Req),
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
    Req = ocpp_message:new_request('BootNotification', Payload),
    {error, Error} = ocpp_station:rpccall(StationId, Req),
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
    Req = ocpp_message:new_request('BootNotification', Payload),
    {error, Error} = ocpp_station:rpccall(StationId, Req),
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
    Req = ocpp_message:new_request('BootNotification', Payload),
    {error, Error} = ocpp_station:rpccall(StationId, Req),
        ?assertEqual(ocpp_message:id(Req), ocpp_error:id(Error)),
    ?assertEqual(<<"InternalError">>, ocpp_error:code(Error)),
    ?assertEqual(<<"An internal error occurred and the receiver "
                   "was not able to process the requested Action "
                   "successfully">>,
                 ocpp_error:description(Error)),
    ?assertEqual(#{<<"reason">> => <<"because exit/1">>}, ocpp_error:details(Error)),
    test_new_boot_request_works(StationId, Payload).

test_new_boot_request_works(StationId, Payload) ->
    NewReq = ocpp_message:new_request(
               'BootNotification',
               Payload#{"customData" =>
                            #{"testAction" => <<"ACCEPT">>,
                              "interval" => 1,
                              "currentTime" => <<"2023-06-15T15:30.00Z">>,
                              "vendorId" => <<"handle_error">>}}),
    ct:log("trying new message..."),
    {ok, Response} = ocpp_station:rpccall(StationId, NewReq),
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
                    ocpp_station:rpccall(
                      Station,
                      ocpp_message:new_request(
                        'BootNotification',
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
                    ocpp_station:rpccall(
                      Station, ocpp_message:new_request('BootNotification', Req))
        end,
    {Pid, Ref} = spawn_monitor(F),
    timer:sleep(100),
    exit(Pid, 'time to die'),
    receive
        {'DOWN', Ref, process, Pid, 'time to die'} ->
            %% Do new connection and request
            ok = ocpp_station:connect(Station),
            Msg = ocpp_message:new_request(
                    'BootNotification',
                    Req#{"customData" =>
                             CustomData#{"testAction" => <<"ACCEPT">>}}),
            {ok, Response} = ocpp_station:rpccall(Station, Msg),
            ?assertEqual(<<"Accepted">>, ocpp_message:get(<<"status">>, Response));
        _ ->
            ct:log("Unnexepcted message received in test case"),
            ct:fail('unnexpected message')
    end.

not_supported_error() ->
    [{doc, "When a request is received that is not implemented "
           "by the handler callback module it responds with a "
           "not implemented error. "},
     {timetrap, 5000}].
not_supported_error(Config) ->
    Station = ?config(stationid, Config),
    Msg = 'Get15118EVCertificate',
    Req = #{"iso15118SchemaVersion" => <<"1">>,
            "action" => <<"Install">>,
            "exiRequest" => <<"abcdefg">>},
    {error, Response} = ocpp_station:rpccall(Station, ocpp_message:new_request(Msg, Req)),
    ?assertEqual(<<"NotSupported">>, ocpp_error:code(Response)).

set_connector_status() ->
    [{doc, "It should be possible to set the status of each connector "
           "multiple times after the station has been accepted"},
     {timetrap, 5000}].
set_connector_status(Config) ->
    StationId = ?config(stationid, Config),
    Connectors = ?config(connectors, Config),
    Conn = lists:flatten(
             [lists:zip(lists:duplicate(NumConn, EVSEId),
                        lists:seq(1, NumConn))
              || {EVSEId, NumConn} <- Connectors]),
    Responses =
        [begin
             Msg = ocpp_message:new_request(
                     'StatusNotification',
                     #{"timestamp" => ?nowutc,
                       "connectorStatus" => <<"Unavailable">>,
                       "evseId" => EVSE,
                       "connectorId" => ConnId}),
             {ocpp_station:rpccall(StationId, Msg), ocpp_message:id(Msg)}
         end
         || {EVSE, ConnId} <- Conn],
    lists:foreach(
      fun ({{ok, Message}, Id}) ->
              ?assertEqual(<<"StatusNotificationResponse">>, ocpp_message:type(Message)),
              ?assertEqual(Id, ocpp_message:id(Message));
          ({{error, Reason}, _}) ->
              ct:fail("Got unexpected error response to status notification: ~p",
                      Reason)
      end,
      Responses),
    NewResponses =
        [begin
             Msg = ocpp_message:new_request(
                     'StatusNotification',
                     #{"timestamp" => ?nowutc,
                       "connectorStatus" => <<"Available">>,
                       "evseId" => EVSE,
                       "connectorId" => ConnId}),
             {ocpp_station:rpccall(StationId, Msg), ocpp_message:id(Msg)}
         end
         || {EVSE, ConnId} <- Conn],
    lists:foreach(
      fun ({{ok, Message}, Id}) ->
              ?assertEqual(<<"StatusNotificationResponse">>, ocpp_message:type(Message)),
              ?assertEqual(Id, ocpp_message:id(Message));
          ({{error, Reason}, _}) ->
              ct:fail("Got unexpected error response to *second* status notification: ~p",
                      Reason)
      end,
      NewResponses).

provision_invalid_evse() ->
    [{timetrap, 5000}].
provision_invalid_evse(Config) ->
    StationId = ?config(stationid, Config),
    Connectors = ?config(connectors, Config),
    {EVSE, _} = lists:unzip(Connectors),
    BadEVSEId = lists:max(EVSE) + 1,
    provision_invalid(StationId, BadEVSEId, 1),
    provision_invalid(StationId, 0, 1).

provision_invalid_connector() ->
    [{timetrap, 5000}].
provision_invalid_connector(Config) ->
    StationId = ?config(stationid, Config),
    [{EVSEId, NumConn}|_] = ?config(connectors, Config),
    BadConn = NumConn + 1,
    provision_invalid(StationId, EVSEId, BadConn),
    provision_invalid(StationId, EVSEId, 0).

provision_invalid(StationId, EVSEId, ConnId) ->
    Msg = ocpp_message:new_request(
            'StatusNotification',
            #{"timestamp" => ?nowutc,
              "connectorStatus" => <<"Available">>,
              "evseId" => EVSEId,
              "connectorId" => ConnId}),
    {error, Reason} = ocpp_station:rpccall(StationId, Msg),
    ?assertEqual(ocpp_message:id(Msg), ocpp_error:id(Reason)),
    ?assertEqual(<<"GenericError">>, ocpp_error:code(Reason)).

forbidden_messages() ->
    [{doc, "Test requirement B02.FR.09 - any message other than a "
           "BootNotificationRequest while in the Pending status "
           "is met with a 'SecurityError' response."},
     {timetrap, 5000}].
forbidden_messages(Config) ->
    StationId = ?config(stationid, Config),
    Msg = ocpp_message:new_request(
            'StatusNotification',
            #{"timestamp" => ?nowutc,
              "connectorStatus" => <<"Available">>,
              "evseId" => 1,
              "connectorId" => 1}),
    {error, Reason} = ocpp_station:rpccall(StationId, Msg),
    ?assertEqual(ocpp_message:id(Msg), ocpp_error:id(Reason)),
    ?assertEqual(<<"SecurityError">>, ocpp_error:code(Reason)).

boot_request_pending() ->
    [{doc, "The station accepts a boot request while in the pending state."},
     {timetrap, 5000}].
boot_request_pending(Config) ->
    StationId = ?config(stationid, Config),
    Msg = ?BOOT_ACCEPT,
    {ok, Response} = ocpp_station:rpccall(StationId, Msg),
    ?assertEqual(ocpp_message:id(Msg), ocpp_message:id(Response)),
    ?assertEqual(<<"Accepted">>, ocpp_message:get(<<"status">>, Response)).

reconnect() ->
    [{doc, "A station can reconnect and send a heartbeat or a status "
           "notification followed by a heartbeat."},
     {timetrap, 5000}].
reconnect(Config) ->
    ConnPid = ?config(connpid, Config),
    exit(ConnPid, abnormal),
    timer:sleep(10),
    ok = ocpp_station:connect(?config(stationid, Config)),
    Req = ocpp_message:new_request(
            'StatusNotification',
            #{"timestamp" => ?nowutc,
              "connectorStatus" => <<"Unavailable">>,
              "evseId" => 1,
              "connectorId" => 1}),
    {ok, Response} = ocpp_station:rpccall(?config(stationid, Config), Req),
    ?assertEqual(ocpp_message:id(Req), ocpp_message:id(Response)),
    ?assertEqual('StatusNotification',
                 ocpp_message:response_type(Response)).

restart() ->
    [{doc, "Simulate the station losing its connection because it rebooted"},
     {timetrap, 5000}].
restart(Config) ->
    ConnPid = ?config(connpid, Config),
    exit(ConnPid, abnormal),
    timer:sleep(10),
    StationId = ?config(stationid, Config),
    ok = ocpp_station:connect(StationId),
    Req = ?BOOT_ACCEPT,
    {ok, Response} = ocpp_station:rpccall(StationId, Req),
    ?assertEqual(ocpp_message:id(Req), ocpp_message:id(Response)),
    ?assertEqual(<<"Accepted">>, ocpp_message:get(<<"status">>, Response)).
