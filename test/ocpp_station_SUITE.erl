%%% Copyright (c) 2023 Will Vining <wfv@vining.dev>
-module(ocpp_station_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-define(stationid(Name), atom_to_binary(Name)).
-define(nowutc,
        list_to_binary(
          calendar:system_time_to_rfc3339(
            erlang:system_time(second),
            [{offset, "Z"}, {unit, second}]))).
-define(BOOT_NOTIFICATION,
        #{chargingStation => #{model => <<"station_test">>,
                               vendorName => <<"common_test">>},
          reason => <<"PowerUp">>}).
-define(BOOT_RESPONSE(Status),
        #{status => Status,
          interval => 10,
          currentTime => ?nowutc}).


%%--------------------------------------------------------------------
%% @spec suite() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds,30}}].

%%--------------------------------------------------------------------
%% @spec init_per_suite(Config0) ->
%%     Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    ok = application:ensure_started(gproc),
    ok = application:ensure_started(jerk),
    load_schemas(),
    Config.

load_schemas() ->
    PrivDir = code:priv_dir(ocpp),
    {ok, Files} = file:list_dir(filename:join(PrivDir, "json_schemas")),
    SchemaFiles = lists:filter(
                    fun(FileName) ->
                            filename:extension(FileName) =:= ".json"
                    end, Files),
    [ok = jerk:load_schema(filename:join([PrivDir, "json_schemas", File]))
     || File <- SchemaFiles].

%%--------------------------------------------------------------------
%% @spec end_per_suite(Config0) -> term() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok = application:stop(gproc),
    ok = application:stop(jerk).

%%--------------------------------------------------------------------
%% @spec init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_group(GroupName, Config0) ->
%%               term() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_testcase(Case, Config)
  when Case =:= boot;
       Case =:= message_before_boot ->
    start_station(?stationid(Case), Config);
init_per_testcase(Case, Config)
 when Case =:= boot_pending;
      Case =:= report_pending;
      Case =:= multiple_reports;
      Case =:= report_rejected;
      Case =:= pending_trigger_message ->
    StationId = ?stationid(Case),
    Config1 = start_station(StationId, Config),
    ok = ocpp_station:connect(StationId),
    boot_station(StationId, <<"Pending">>),
    Config1;
init_per_testcase(boot_accepted, Config) ->
    StationId = ?stationid(boot_accepted),
    Config1 = start_station(StationId, Config),
    ok = ocpp_station:connect(StationId),
    boot_station(StationId, <<"Accepted">>),
    Config1;
init_per_testcase(Case, Config)
  when Case =:= report_idle;
       Case =:= sync_call ->
    StationId = ?stationid(Case),
    Config1 = start_station(StationId, Config),
    ok = ocpp_station:connect(StationId),
    provision_idle(StationId),
    Config1;
init_per_testcase(Case, Config)
  when Case =:= get_variables;
       Case =:= set_variables ->
    StationId = ?stationid(Case),
    Config1 = start_station(StationId, Config),
    ok = ocpp_station:connect(StationId),
    boot_station(StationId, <<"Pending">>),
    Config1;
init_per_testcase(Case, Config)
  when Case =:= offline_reboot;
       Case =:= offline_reconnect ->
    StationId = ?stationid(Case),
    start_station(StationId, Config);
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_testcase(TestCase, Config0) ->
%%               term() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
end_per_testcase(TestCase, Config)
  when TestCase =:= boot;
       TestCase =:= message_before_boot;
       TestCase =:= boot_pending;
       TestCase =:= boot_accepted;
       TestCase =:= offline_reboot;
       TestCase =:= offline_reconnect;
       TestCase =:= report_pending;
       TestCase =:= report_idle;
       TestCase =:= report_rejected;
       TestCase =:= multiple_reports;
       TestCase =:= get_variables;
       TestCase =:= set_variables;
       TestCase =:= sync_call;
       TestCase =:= pending_trigger_message ->
    Sup = ?config(stationsup, Config),
    unlink(Sup),
    ocpp_station_sup:stop(Sup),
    ok;
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%% @end
%%--------------------------------------------------------------------
groups() ->
    [{provisioning, [parallel],
      [boot,
       message_before_boot,
       boot_pending,
       boot_accepted,
       pending_trigger_message,
       {group, offline},
       {group, configure}]},
     {offline, [parallel], [offline_reconnect, offline_reboot]},
     {configure, [parallel],
      [report_pending,
       report_idle,
       multiple_reports,
       report_rejected,
       get_variables,
       set_variables]}].

%%--------------------------------------------------------------------
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
all() ->
    [{group, provisioning}, sync_call].

boot() ->
    [{timetrap, 5000}].
boot(Config) ->
    StationId = ?config(stationid, Config),
    %% run the reject test in a different process so we can test the
    %% case where the station disconnects after a rejected boot
    %% request.
    {Pid, Ref} = spawn_monitor(fun () -> boot_reject(StationId) end),
    receive
        {'DOWN', Ref, process, Pid, normal} ->
            ok;
        {'DOWN', Ref, process, Pid, Reason} ->
            ct:fail("boot_reject failed: ~p", [Reason])
    end,
    %% The remaining tests won't disocnnect
    ok = ocpp_station:connect(StationId),
    boot_station(StationId, <<"Pending">>),
    boot_station(StationId, <<"Accepted">>).

boot_reject(StationId) ->
    ok = ocpp_station:connect(StationId),
    BootNotification =
        ocpp_message:new_request('BootNotification', ?BOOT_NOTIFICATION),
    ok = ocpp_station:rpccall(StationId, BootNotification),
    BootResponse =
        ocpp_message:new_response(
          'BootNotification', ?BOOT_RESPONSE(<<"Rejected">>),
          ocpp_message:id(BootNotification)),
    ok = ocpp_station:reply(StationId, BootResponse),
    receive_ocpp(ocpp_message:id(BootResponse), rpcreply).

message_before_boot(Config) ->
    StationId = ?config(stationid, Config),
    ok = ocpp_station:connect(StationId),
    HeartbeatMsg = ocpp_message:new_request('Heartbeat', #{}),
    ok = ocpp_station:rpccall(StationId, HeartbeatMsg),
    receive_ocpp(ocpp_message:id(HeartbeatMsg), rpcerror,
                 fun(Msg) -> ocpp_error:code(Msg) =:= <<"SecurityError">> end).

boot_pending() ->
    [{"manipulate the station while in the pending state"},
     {timetrap, 5000}].
boot_pending(Config) ->
    %% Sending any unsolicited RPCCALL other than a BootNotification
    %% causes a security error.
    StationId = ?config(stationid, Config),
    HeartbeatMsg = ocpp_message:new_request('Heartbeat', #{}),
    ok = ocpp_station:rpccall(StationId, HeartbeatMsg),
    receive_ocpp(
      ocpp_message:id(HeartbeatMsg), rpcerror,
      fun(Msg) -> ocpp_error:code(Msg) =:= <<"SecurityError">> end),

    BootNotification =
        ocpp_message:new_request('BootNotification', ?BOOT_NOTIFICATION),
    ok = ocpp_station:rpccall(StationId, BootNotification),
    BootResponse =
        ocpp_message:new_response('BootNotification', ?BOOT_RESPONSE(<<"Accepted">>),
                                  ocpp_message:id(BootNotification)),
    ok = ocpp_station:reply(StationId, BootResponse),
    receive_ocpp(ocpp_message:id(BootResponse), rpcreply,
                 fun(Msg) -> ocpp_message:get(<<"status">>, Msg) =:= <<"Accepted">> end).

boot_accepted() ->
    [{doc, "recieve StatusNotification messages after station is accepted"},
     {timetrap, 5000}].
boot_accepted(Config) ->
    StationId = ?config(stationid, Config),
    StatusNotification =
        ocpp_message:new_request(
          'StatusNotification',
          #{timestamp => ?nowutc,
            connectorStatus => <<"Available">>,
            evseId => 1,
            connectorId => 1}),
    ok = ocpp_station:rpccall(StationId, StatusNotification),
    receive_ocpp(ocpp_message:id(StatusNotification), rpcreply),
    %% Follow up with a heartbeat request
    send_heartbeat(StationId).

offline_reconnect() ->
    [{doc, "handle station that goes offline wtihout rebooting"},
     {timetrap, 5000}].
offline_reconnect(Config) ->
    StationId = ?config(stationid, Config),
    offline_idle(StationId),
    ok = ocpp_station:connect(StationId),
    send_heartbeat(StationId).

offline_reboot() ->
    [{doc, "handle station that goes offline due to an unexpected reboot"},
     {timetrap, 5000}].
offline_reboot(Config) ->
    StationId = ?config(stationid, Config),
    offline_idle(StationId),
    {Pid, Ref} = spawn_monitor(
                   fun() ->
                           ok = ocpp_station:connect(StationId),
                           boot_station(StationId, <<"Accepted">>)
                   end),
    receive
        {'DOWN', Ref, process, Pid, normal} ->
            ok;
        {'DOWN', Ref, process, Pid, Reason} ->
            ct:fail("unexpected reboot test failed: ~p", [Reason])
    end,
    ok = ocpp_station:connect(StationId),
    boot_station(StationId, <<"Accepted">>),
    send_heartbeat(StationId).

offline_idle(StationId) ->
    {Pid, Ref} = spawn_monitor(
                   fun() ->
                           ok = ocpp_station:connect(StationId),
                           provision_idle(StationId)
                   end),
    receive
        {'DOWN', Ref, process, Pid, normal} ->
            ok;
        {'DOWN', Ref, process, Pid, Reason} ->
            ct:fail("offline_idle failed: ~p", [Reason])
    end.

report_pending() ->
    [{doc, "handle report requests and notifications in the pending state"},
     {timetrap, 5000}].
report_pending(Config) ->
    StationId = ?config(stationid, Config),
    %% sending an unsolicited ReportNotification results in a security error
    ReportNotification =
        ocpp_message:new_request(
          'NotifyReport',
          #{requestId => 1,
            generatedAt => ?nowutc,
            tbc => true,
            seqNo => 0}),
    ok = ocpp_station:rpccall(StationId, ReportNotification),
    receive_ocpp(ocpp_message:id(ReportNotification), rpcerror,
                 fun(Msg) -> ocpp_error:code(Msg) =:= <<"SecurityError">> end),
    solicit_base_report(StationId, [{requestid, 1}, {status, <<"Accepted">>}]),
    ok = ocpp_station:rpccall(StationId, ReportNotification),
    receive_ocpp(ocpp_message:id(ReportNotification), rpcreply,
                 fun (Msg) ->
                         ocpp_message:response_type(Msg) =:= 'NotifyReport'
                 end),
    %% Send a second part
    ReportNotification1 =
        ocpp_message:new_request(
          'NotifyReport',
          #{requestId => 1,
            generatedAt => ?nowutc,
            tbc => false,
            seqNo => 1}),
    ok = ocpp_station:rpccall(StationId, ReportNotification1),
    receive_ocpp(ocpp_message:id(ReportNotification1), rpcreply,
                 fun (Msg) ->
                         ocpp_message:response_type(Msg) =:= 'NotifyReport'
                 end),
    %% A subsequent part (after tbc=>false) should again result in a security error
    ReportNotification2 =
        ocpp_message:new_request(
          'NotifyReport',
          #{requestId => 1,
            generatedAt => ?nowutc,
            tbc => false,
            seqNo => 2}),
    ok = ocpp_station:rpccall(StationId, ReportNotification2),
    receive_ocpp(ocpp_message:id(ReportNotification2), rpcerror,
                 fun(Msg) -> ocpp_error:code(Msg) =:= <<"SecurityError">> end).

report_idle() ->
    [{doc, "can process reports in the idle state."},
     {timetrap, 1000}].
report_idle(Config) ->
    StationId = ?config(stationid, Config),
    solicit_base_report(StationId, [{requestid, 1}, {status, <<"Accepted">>}]),
    ReportNotification =
        ocpp_message:new_request(
          'NotifyReport',
          #{requestId => 1,
            generatedAt => ?nowutc,
            tbc => true,
            seqNo => 0,
            reportData => [#{component => #{name => <<"component">>,
                                            evse => #{id => 1, connectorId => 1}},
                             variable => #{name => <<"foo">>,
                                           instance => <<"bar">>},
                             variableAttribute => [#{type => 'Actual', value => <<"1.2">>,
                                                     mutability => <<"ReadOnly">>},
                                                   #{type => 'Target', value => <<"1.0">>}],
                             variableCharacteristics =>
                                 #{dataType => <<"decimal">>,
                                   supportsMonitoring => false}}]}),
    ok = ocpp_station:rpccall(StationId, ReportNotification),
    receive_ocpp(ocpp_message:id(ReportNotification), rpcreply,
                 fun (Msg) ->
                         ocpp_message:response_type(Msg) =:= 'NotifyReport'
                 end),
    %% Send a second part
    ReportNotification1 =
        ocpp_message:new_request(
          'NotifyReport',
          #{requestId => 1,
            generatedAt => ?nowutc,
            tbc => false,
            seqNo => 1,
            reportData => [#{component => #{name => <<"component">>,
                                            evse => #{id => 1, connectorId => 1}},
                             variable => #{name => <<"foo">>,
                                           instance => <<"baz">>},
                             variableAttribute => [#{type => 'Actual', value => <<"1.5">>,
                                                     mutability => <<"ReadOnly">>}],
                             variableCharacteristics =>
                                 #{dataType => <<"string">>,
                                   supportsMonitoring => false}}]}),
    ok = ocpp_station:rpccall(StationId, ReportNotification1),
    receive_ocpp(ocpp_message:id(ReportNotification1), rpcreply,
                 fun (Msg) ->
                         ocpp_message:response_type(Msg) =:= 'NotifyReport'
                 end),
    {ok, <<"1.5">>} = ocpp_station:lookup_variable(
                        StationId,
                        "Component", "FOO",
                        [{variable_instance, "Baz"}, {evse, 1}, {connector, 1}],
                        'Actual'),
    {ok, 1.2} = ocpp_station:lookup_variable(
                  StationId,
                  "Component", "Foo",
                  [{variable_instance, "bar"}, {evse, 1}, {connector, 1}],
                  'Actual'),
    {ok, 1.0} = ocpp_station:lookup_variable(
                  StationId,
                  "Component", "Foo",
                  [{variable_instance, "bar"}, {evse, 1}, {connector, 1}],
                  'Target').

multiple_reports() ->
    [{doc, "it is possible to request and receive multiple reports concurrently"},
     {timetrap, 5000}].
multiple_reports(Config) ->
    StationId = ?config(stationid, Config),
    solicit_base_report(StationId, [{requestid, 1}]),
    solicit_base_report(StationId, [{requestid, 2}]),
    MessageBody = #{requestId => 1, generatedAt => ?nowutc, tbc => true, seqNo => 0},
    RepNot1 = ocpp_message:new_request('NotifyReport', MessageBody#{tbc => false}),
    RepNot2 = ocpp_message:new_request('NotifyReport', MessageBody#{requestId => 2}),
    ok = ocpp_station:rpccall(StationId, RepNot2),
    IsNotifyReport = fun(Msg) -> ocpp_message:response_type(Msg) =:= 'NotifyReport' end,
    receive_ocpp(ocpp_message:id(RepNot2), rpcreply, IsNotifyReport),
    ok = ocpp_station:rpccall(StationId, RepNot1),
    receive_ocpp(ocpp_message:id(RepNot1), rpcreply, IsNotifyReport),
    RepNot3 = ocpp_message:new_request(
                'NotifyReport', MessageBody#{requestId => 2, tbc => false, seqNo => 1}),
    ok = ocpp_station:rpccall(StationId, RepNot3),
    receive_ocpp(ocpp_message:id(RepNot3), rpcreply, IsNotifyReport).

report_rejected() ->
    [{doc, "if a station in the pending state sends a NotifyReportRequest for "
           "a GetReportRequest that it rejected it results in a security error."},
     {timetrap, 5000}].
report_rejected(Config) ->
    StationId = ?config(stationid, Config),
    solicit_base_report(StationId, [{status, <<"Rejected">>}, {requestid, 1}]),
    ReportNotification =
        ocpp_message:new_request(
          'NotifyReport',
          #{requestId => 1, generatedAt => ?nowutc, tbc => false, seqNo => 0}),
    ok = ocpp_station:rpccall(StationId, ReportNotification),
    receive_ocpp(ocpp_message:id(ReportNotification), rpcerror,
                 fun(Msg) -> ocpp_error:code(Msg) =:= <<"SecurityError">> end).

get_variables() ->
    [{doc, "when variables are requested from a charging station the "
           "station device model is updated."},
     {timetrap, 5000}].
get_variables(Config) ->
    StationId = ?config(stationid, Config),
    GetVariablesRequest =
        ocpp_message:new_request('GetVariables',
                                 #{getVariableData => [#{attributeType => 'Actual',
                                                         component => #{name => <<"OCPPCommCtrlr">>},
                                                         variable => #{name => <<"WebsocketPingInterval">>}},
                                                       #{attributeType => 'Actual',
                                                         component => #{name => <<"Connector">>,
                                                                        evse => #{id => 1, connectorId => 2}},
                                                         variable => #{name => <<"AvailabilityState">>}},
                                                       #{attributeType => 'Target',
                                                         component => #{name => <<"EVSE">>,
                                                                        evse => #{id => 1, connectorId => 1}},
                                                         variable => #{name => <<"Power">>}}]}),
    ocpp_station:call(StationId, GetVariablesRequest),
    receive_ocpp(ocpp_message:id(GetVariablesRequest), rpccall,
                 fun(Msg) -> ocpp_message:request_type(Msg) =:= 'GetVariables' end),
    GetVariablesResponse =
        ocpp_message:new_response(
          'GetVariables',
          #{getVariableResult => [#{attributeStatus => <<"Accepted">>,
                                    attributeType => 'Actual',
                                    attributeValue => <<"120">>,
                                    component => #{name => <<"OCPPCommCtrlr">>},
                                    variable => #{name => <<"WebSocketPingInterval">>}},
                                  #{attributeStatus => <<"UnknownComponent">>,
                                    attributeType => 'Actual',
                                    component => #{name => <<"Connector">>, evse => #{id => 1, connectorId => 2}},
                                    variable => #{name => <<"AvailabilityState">>}},
                                  #{attributeStatus => <<"Accepted">>,
                                    attributeType => 'Target',
                                    attributeValue => <<"">>,
                                    component => #{name => <<"EVSE">>, evse => #{id => 1, connectorId => 1}},
                                    variable => #{name => <<"Power">>}}]},
          ocpp_message:id(GetVariablesRequest)),
    ocpp_station:rpcreply(StationId, GetVariablesResponse),
    {ok, 120} = ocpp_station:lookup_variable(StationId, "OCPPCommCtrlr", "WebSocketPingInterval", [], 'Actual'),
    {error, undefined} = ocpp_station:lookup_variable(StationId, "Connector", "AvailabilityState", [{evse, 1}, {connector, 2}], 'Actual'),
    {ok, undefined} = ocpp_station:lookup_variable(StationId, "EVSE", "Power", [{evse, 1}, {connector, 1}], 'Target').

set_variables() ->
    [{doc, "When a set variable request is accepted the device model is updated. "
           "When a ser variable request is rejected the device model is not updated."},
     {timetrap, 5000}].
set_variables(Config) ->
    StationId = ?config(stationid, Config),
    SetVariablesRequest =
        ocpp_message:new_request('SetVariables',
                                 #{setVariableData => [#{component => #{name => <<"OCPPCommCtrlr">>},
                                                         variable => #{name => <<"WebsocketPingInterval">>},
                                                         attributeValue => <<"2">>},
                                                       #{component => #{name => <<"EVSE">>,
                                                                        evse => #{id => 1, connectorId => 1}},
                                                         variable => #{name => <<"Power">>},
                                                         attributeValue => <<"100">>}]}),
    ocpp_station:call(StationId, SetVariablesRequest),
    receive_ocpp(ocpp_message:id(SetVariablesRequest), rpccall,
                 fun(Msg) -> ocpp_message:request_type(Msg) =:= 'SetVariables' end),
    SetVariablesResponse =
        ocpp_message:new_response(
          'SetVariables',
          #{setVariableResult => [#{attributeStatus => <<"Accepted">>,
                                    component => #{name => <<"OCPPCommCtrlr">>},
                                    variable => #{name => <<"WebSocketPingInterval">>}},
                                  #{attributeStatus => <<"Rejected">>,
                                    component => #{name => <<"EVSE">>,
                                                   evse => #{id => 1, connectorId => 1}},
                                    variable => #{name => <<"Power">>}}]},
          ocpp_message:id(SetVariablesRequest)),
    ocpp_station:rpcreply(StationId, SetVariablesResponse),
    {ok, 2} = ocpp_station:lookup_variable(StationId, "OCPPCommCtrlr", "WebSocketPingInterval", [], 'Actual'),
    {error, undefined} = ocpp_station:lookup_variable(StationId, "EVSE", "Power", [{evse, 1}, {connector, 1}], 'Actual').

sync_call() ->
    [{doc, "A synchronous call times out when no response is received from the station"},
     {timetrap, 5000}].
sync_call(Config) ->
    StationId = ?config(stationid, Config),
    SetVariablesRequest =
        ocpp_message:new_request('SetVariables',
                                 #{setVariableData => [#{component => #{name => <<"OCPPCommCtrlr">>},
                                                         variable => #{name => <<"WebsocketPingInterval">>},
                                                         attributeValue => <<"2">>},
                                                       #{component => #{name => <<"EVSE">>,
                                                                        evse => #{id => 1, connectorId => 1}},
                                                         variable => #{name => <<"Power">>},
                                                         attributeValue => <<"100">>}]}),
    ?assertExit({timeout, _}, ocpp_station:call_sync(StationId, SetVariablesRequest, 50)),
    SetVariablesResponse =
        ocpp_message:new_response(
          'SetVariables',
          #{setVariableResult => [#{attributeStatus => <<"Accepted">>,
                                    component => #{name => <<"OCPPCommCtrlr">>},
                                    variable => #{name => <<"WebSocketPingInterval">>}},
                                  #{attributeStatus => <<"Rejected">>,
                                    component => #{name => <<"EVSE">>,
                                                   evse => #{id => 1, connectorId => 1}},
                                    variable => #{name => <<"Power">>}}]},
          ocpp_message:id(SetVariablesRequest)),
    timer:apply_after(50, ocpp_station, rpcreply, [StationId, SetVariablesResponse]),
    ok = ocpp_station:call_sync(StationId, SetVariablesRequest, 200).

pending_trigger_message() ->
    [{doc, "Certain messages can be triggered by the CSMS while the station is in the pending state"},
     {timetrap, 5000}].
pending_trigger_message(Config) ->
    StationId = ?config(stationid, Config),
    %% Without a trigger request the message results in a security error.
    Heartbeat1 = ocpp_message:new_request('Heartbeat', #{}),
    ok = ocpp_station:rpccall(StationId, Heartbeat1),
    receive_ocpp(ocpp_message:id(Heartbeat1), rpcerror,
                 fun(Msg) -> ocpp_error:code(Msg) =:= <<"SecurityError">> end),
    ct:pal("first test passed"),

    TriggerMessage1 = ocpp_message:new_request('TriggerMessage', #{requestedMessage => <<"Heartbeat">>}),
    ocpp_station:call(StationId, TriggerMessage1),
    receive_ocpp(ocpp_message:id(TriggerMessage1), rpccall),
    ocpp_station:rpcreply(
      StationId,
      ocpp_message:new_response('TriggerMessage',
                                #{status => <<"Rejected">>}, ocpp_message:id(TriggerMessage1))),
    Heartbeat2 = ocpp_message:new_request('Heartbeat', #{}),
    ok = ocpp_station:rpccall(StationId, Heartbeat2),
    receive_ocpp(ocpp_message:id(Heartbeat2), rpcerror,
                 fun(Msg) -> ocpp_error:code(Msg) =:= <<"SecurityError">> end),
    ct:pal("second test passed"),

    TriggerMessage2 = ocpp_message:new_request('TriggerMessage', #{requestedMessage => <<"Heartbeat">>}),
    ocpp_station:call(StationId, TriggerMessage2),
    receive_ocpp(ocpp_message:id(TriggerMessage2), rpccall),
    ocpp_station:rpcreply(
      StationId,
      ocpp_message:new_response('TriggerMessage',
                                #{status => <<"Accepted">>}, ocpp_message:id(TriggerMessage2))),
    Heartbeat3 = ocpp_message:new_request('Heartbeat', #{}),
    ok = ocpp_station:rpccall(StationId, Heartbeat3),
    receive_ocpp(ocpp_message:id(Heartbeat3), rpcreply).


%%% Helper functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

boot_station(StationId, Status) ->
    BootNotification =
        ocpp_message:new_request('BootNotification', ?BOOT_NOTIFICATION),
    ok = ocpp_station:rpccall(StationId, BootNotification),
    BootResponse =
        ocpp_message:new_response(
          'BootNotification', ?BOOT_RESPONSE(Status),
          ocpp_message:id(BootNotification)),
    ok = ocpp_station:reply(StationId, BootResponse),
    receive_ocpp(ocpp_message:id(BootResponse), rpcreply,
                 fun(Msg) -> ocpp_message:get(<<"status">>, Msg) =:= Status end).

provision_idle(StationId) ->
    boot_station(StationId, <<"Accepted">>),
    send_heartbeat(StationId).

receive_ocpp(MessageId, MessageType) ->
    receive_ocpp(MessageId, MessageType, fun(_) -> true end).

receive_ocpp(MessageId, MessageType, Pred) ->
    receive
        {ocpp, {rpcerror, Message}} when MessageType =:= rpcerror ->
            ?assertEqual(MessageId, ocpp_error:id(Message)),
            ?assert(Pred(Message));
        {ocpp, {MessageType, Message}} when MessageType =/= rpcerror ->
            ?assertEqual(MessageId, ocpp_message:id(Message)),
            ?assert(Pred(Message));
        Anything ->
            ct:fail("unexpected message received: ~p", [Anything])
    end.

send_heartbeat(StationId) ->
    Heartbeat = ocpp_message:new_request('Heartbeat', #{}),
    ok = ocpp_station:rpccall(StationId, Heartbeat),
    receive_ocpp(ocpp_message:id(Heartbeat), rpcreply,
                 fun(Msg) -> ocpp_message:response_type(Msg) =:= 'Heartbeat' end).

solicit_base_report(StationId, Options) ->
    Status = proplists:get_value(status, Options, <<"Accepted">>),
    ReqId = proplists:get_value(requestid, Options, 1),
    GetReport =
        ocpp_message:new_request(
          'GetBaseReport', #{requestId => ReqId, reportBase => <<"FullInventory">>}),
    ok = ocpp_station:call(StationId, GetReport),
    receive_ocpp(ocpp_message:id(GetReport), rpccall,
                 fun (Msg) ->
                         (ocpp_message:request_type(Msg) =:= 'GetBaseReport')
                             and (ocpp_message:get(<<"requestId">>, Msg) =:= ReqId)
                 end),
    GetReportResponse =
        ocpp_message:new_response(
          'GetBaseReport', #{status => Status}, ocpp_message:id(GetReport)),
    ocpp_station:rpcreply(StationId, GetReportResponse).

start_station(StationId, Config) ->
    {ok, Sup} = ocpp_station_sup:start_link(StationId, [ocpp_evse:new(1)]),
    [{stationid, StationId}, {stationsup, Sup} | Config].
