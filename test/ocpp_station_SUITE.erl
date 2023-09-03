%%% Copyright (c) 2023 Will Vining <wfv@vining.dev>
-module(ocpp_station_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-define(stationid(Name), atom_to_binary(Name)).
-define(BOOT_NOTIFICATION,
        #{chargingStation => #{model => <<"station_test">>,
                               vendorName => <<"common_test">>},
          reason => <<"PowerUp">>}).
-define(BOOT_RESPONSE(Status),
        #{status => Status,
          interval => 10,
          currentTime =>
              list_to_binary(calendar:system_time_to_rfc3339(
                               erlang:system_time(second),
                               [{offset, "Z"}, {unit, second}]))}).


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
init_per_testcase(boot_pending, Config) ->
    StationId = ?stationid(boot_pending),
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
       TestCase =:= offline_reconnect ->
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
       {group, offline},
       {group, configure}]},
     {offline, [parallel], [offline_reconnect, offline_reboot]},
     {configure, []}].

%%--------------------------------------------------------------------
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
all() ->
    [{group, provisioning}].

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
          #{timestamp => list_to_binary(
                           calendar:system_time_to_rfc3339(
                             erlang:system_time(second), [{offset, "Z"}, {unit, second}])),
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

start_station(StationId, Config) ->
    {ok, Sup} = ocpp_station_sup:start_link(StationId, [ocpp_evse:new(1)]),
    [{stationid, StationId}, {stationsup, Sup} | Config].
