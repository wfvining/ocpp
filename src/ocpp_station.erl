%%% @doc representation of a charging station within the CSMS.
%%%
%%% Several API functions come in pairs where one function is prefixed
%%% with `rpc'. These pairs differentiate between messages flowing
%%% from the CSMS to the station and messages flowing from the station
%%% to the CSMS. Functions with the `rpc' prefix (e.g. `rpccall') are
%%% used to notify the state machine of messages arriving at the CSMS
%%% while functions without the prefix (e.g. `call/2') are used to
%%% send messages from the CSMS to the station.
%%%
%%% @end
%%%
%%% Copyright (c) 2023 Will Vining <wfv@vining.dev>

-module(ocpp_station).

-behaviour(gen_statem).

-export([start_link/2, stop/1,
         call/2, call/3, rpccall/2,
         reply/2, rpcreply/2,
         error/2, %rpcerror/2,
         lookup_variable/5,
         connect/1]).

-export([init/1, callback_mode/0, code_change/3, terminate/3]).

%% State functions
-export([disconnected/3,
         connected/3,
         booting/3,
         boot_pending/3,
         provisioning/3,
         idle/3,
         offline/3,
         reconnecting/3,
         resetting/3,
         reset_accepted/3,
         reset_scheduled/3]).

-define(registry(Module, Name), {via, gproc, {n, l, {Module, Name}}}).
-define(registry(Name), ?registry(?MODULE, Name)).

-record(data,
        {stationid :: binary(),
         connection = disconnected :: disconnected | {pid(), reference()},
         evse = #{} :: #{pos_integer() => ocpp_evse:evse()},
         pending = undefined :: undefined | ocpp_message:messageid(),
         pending_call = undefined :: undefined | {timer:tref(), ocpp_message:messageid()},
         pending_report = undefined :: undefined | {ocpp_message:messageid(), integer()},
         expecting_report = [] :: [integer()],
         device_model = ocpp_device_model:new() :: ocpp_device_model:device_model()}).

-spec start_link(StationId :: binary(),
                 EVSE :: [ocpp_evse:evse()]) -> gen_statem:start_ret().
start_link(StationId, EVSE) ->
    gen_statem:start_link(
      ?registry(StationId), ?MODULE, {StationId, EVSE}, []).

%% @doc
%% Connect the calling process to the station. Called by the process
%% responsible for comminication with the physical charging station.
%% @end
-spec connect(Station :: binary())
             -> ok | {error, already_connected}.
connect(Station) ->
    gen_statem:call(?registry(Station), {connect, self()}).

%% @doc Handle an OCPP remote procedure call from the station.
-spec rpccall(Station :: binary(), Request :: ocpp_message:message()) -> ok.
rpccall(Station, Request) ->
    MessageType = ocpp_message:request_type(Request),
    MessageId = ocpp_message:id(Request),
    gen_statem:cast(?registry(Station), {rpccall, MessageType, MessageId, Request}).

%% @doc Reply to an RPC call with a normal response.
-spec reply(StationId :: binary(), Response :: ocpp_message:message()) ->
          ok.
reply(StationId, Response) ->
    gen_statem:cast(?registry(StationId), {reply, Response}).

%% @equiv call_async(Station, Request, 30000)
-spec call(Station :: binary(), Request :: ocpp_message:message()) ->
          ok |
          {error, Reason :: busy | offline}.
call(Station, Request) ->
    call(Station, Request, 30000).

%% @doc Send an RPCCALL message to the station. If there is an
%% outstanding request that has already been sent to the station this
%% will return `{error, busy}'. If the station is not connected
%% `{error, offline}' will be returned. The call will time out after
%% `Timeout' milliseconds.
%%
%% The calling process returns immediately. When an RPCREPLY is
%% received, or a timeout occurs the caller will be notified via an
%% ocpp_handler event.
-spec call(Station :: binary(),
           Request :: ocpp_message:message(),
           Timeout :: pos_integer()) ->
          ok |
          {error, Reason :: busy | offline}.
call(Station, Request, Timeout) ->
    gen_statem:call(?registry(Station), {send_request, Request, Timeout}).

%% @doc Notify the state machine that an RPCREPLY has arrived.
-spec rpcreply(Station :: binary(), Reply :: ocpp_message:message()) ->
          ok.
rpcreply(Station, Response) ->
    MessageType = ocpp_message:response_type(Response),
    MessageId = ocpp_message:id(Response),
    gen_statem:cast(?registry(Station), {rpcreply, MessageType, MessageId, Response}).

%% @doc Reply to an RPC call with an error.
-spec error(StationId :: binary(), Error :: ocpp_error:error()) -> ok.
error(StationId, Error) ->
    gen_statem:cast(?registry(StationId), {error, Error}).

-spec lookup_variable(StationId :: binary(),
                      ComponentName :: binary(),
                      VariableName :: binary(),
                      VariableIdentifiers :: ocpp_device_model:variable_identifiers(),
                      AttributeType :: ocpp_device_model:attribute_type()) ->
          {ok, any()} | {error, ambiguous | undefined}.
lookup_variable(StationId, ComponentName, VariableName, VariableIdentifiers, AttributeType) ->
    gen_statem:call(
      ?registry(StationId),
      {lookup_variable,
       ComponentName,
       VariableName,
       VariableIdentifiers,
       AttributeType}).

-spec stop(Station :: pid()) -> ok.
stop(Station) ->
    gen_statem:stop(Station).

callback_mode() -> state_functions.

init({StationId, EVSESpecs}) ->
    process_flag(trap_exit, true),
    {ok, disconnected, #data{stationid = StationId,
                             evse = init_evse(EVSESpecs)}}.

disconnected({call, From}, {connect, ConnectionPid}, Data) ->
    {next_state, connected,
     setup_connection(Data, ConnectionPid),
     [{reply, From, ok}]};
disconnected({call, From}, {send_request, _, _}, _Data) ->
    {keep_state_and_data, [{reply, From, {error, not_connected}}]}.

connected({call, From}, {connect, _}, _Data) ->
    {keep_state_and_data, [{reply, From, {error, already_connected}}]};
connected(cast, disconnect, Data) ->
    {next_state, disconnected, cleanup_connection(Data)};
connected(cast, {rpccall, 'BootNotification', _, Message}, Data) ->
    NewData = handle_rpccall(Message, Data),
    {next_state, booting, NewData};
connected(cast, {rpccall, _, MessageId, _}, Data) ->
    Error = ocpp_error:new('SecurityError', MessageId),
    ok = send_error(Error, Data),
    keep_state_and_data;
connected({call, From}, {send_request, _, _}, _Data) ->
    {keep_state_and_data, [{reply, From, {error, not_accepted}}]};
connected(EventType, Event, Data) ->
    handle_event(EventType, Event, Data).

booting(cast, disconnect, Data) ->
    {next_state, disconnected, cleanup_connection(Data)};
booting(cast, {reply, Response},
        #data{pending = MessageId} = Data) ->
    ResponseId = ocpp_message:id(Response),
    if
        MessageId =:= ResponseId ->
            NextState = boot_status_to_state(Response),
            send_response(Response, Data),
            {next_state, NextState, clear_pending_request(Data)};
        MessageId =/= ResponseId ->
            logger:notice(
              "Received out of order response to message ~p "
              "while awaiting response to message ~p.  "
              "Message dropped.",
              [ResponseId, MessageId]),
            keep_state_and_data
    end;
booting(cast, {error, Error}, #data{pending = MessageId} = Data) ->
    ResponseId = ocpp_error:id(Error),
    if
        MessageId =:= ResponseId ->
            ok = send_error(Error, Data),
            {next_state, connected, clear_pending_request(Data)};
        MessageId =/= ResponseId ->
            logger:notice(
              "Received out of order error for message ~p "
              "while awaiting response to message ~p.  "
              "Message dropped.",
              [ResponseId, MessageId]),
            keep_state_and_data
    end;
booting(EventType, Event, Data) ->
    handle_event(EventType, Event, Data).

boot_pending({call, From}, {send_request, Message, Timeout}, Data) ->
    case ocpp_message:request_type(Message) of
        MessageType
          when MessageType =:= 'SetVariables';
               MessageType =:= 'GetVariables';
               MessageType =:= 'GetBaseReport';
               MessageType =:= 'GetReport';
               MessageType =:= 'TriggerMessage' ->
            {Reply, NewData} = call_station(Message, Data, Timeout),
            {keep_state, NewData, [{reply, From, Reply}]};
        _ ->
            {keep_state_and_data, [{reply, From, {error, illegal_request}}]}
    end;
boot_pending(cast, disconnect, Data) ->
    {next_state, disconnected, cleanup_connection(Data)};
boot_pending(cast, {rpccall, 'BootNotification', _, _}, Data) ->
    {next_state, connected, Data, [postpone]};
boot_pending(cast, {rpccall, 'NotifyReport', MessageId, Message},
             #data{expecting_report = ExpectedReports} = Data) ->
    RequestId = ocpp_message:get(<<"requestId">>, Message),
    case lists:member(RequestId, ExpectedReports) of
        true ->
            Response = ocpp_message:new_response('NotifyReport', #{}, MessageId),
            send_response(Response, Data),
            {keep_state, process_report(Message, Data)};
        false ->
            Error = ocpp_error:new('SecurityError', MessageId),
            ok = send_error(Error, Data),
            keep_state_and_data
    end;
boot_pending(cast, {rpccall, _, _, Message}, Data) ->
    Error =
        ocpp_error:new(
          'SecurityError',
          ocpp_message:id(Message),
          [{description,
            <<"Unless requested, the charging station is not allowed to initiate "
              "sending any messages other than a BootNotificationRequest "
              "before being accepted.">>}]),
    ok = send_error(Error, Data),
    %% XXX It is not clear whether this is the correct next state.
    keep_state_and_data;
boot_pending(EventType, Event, Data) ->
    handle_event(EventType, Event, Data).

provisioning(cast, {rpccall, 'Heartbeat', MessageId, _}, Data) ->
    send_response(heartbeat_response(MessageId), Data),
    {next_state, idle, Data};
provisioning(cast, {rpccall, 'StatusNotification', MessageId, Message}, Data) ->
    case update_status(Message, Data) of
        {ok, NewData} ->
            Response =
                ocpp_message:new_response('StatusNotification', #{}, MessageId),
            send_response(Response, Data),
            UpdatedData = NewData;
        {error, _} ->
            Response = ocpp_error:new('GenericError', MessageId),
            send_error(Response, Data),
            UpdatedData = Data
    end,
    {next_state, provisioning, UpdatedData};
provisioning(cast, disconnect, Data) ->
    %% The station has already been accepted here so transition is the
    %% same as for `idle'
    {next_state, offline, cleanup_connection(Data)};
provisioning(EventType, Event, Data) ->
    handle_event(EventType, Event, Data).

idle({call, From}, {send_request, Message, Timeout}, Data) ->
    {Reply, NewData} = call_station(Message, Data, Timeout),
    {keep_state, NewData, [{reply, From, Reply}]};
idle(cast, {rpccall, 'NotifyReport', MessageId, Message},
     #data{expecting_report = ExpectedReports} = Data) ->
    RequestId = ocpp_message:get(<<"requestId">>, Message),
    case lists:member(RequestId, ExpectedReports) of
        true ->
            Response = ocpp_message:new_response('NotifyReport', #{}, MessageId),
            send_response(Response, Data),
            {keep_state, process_report(Message, Data)};
        false ->
            logger:notice(
              "Received unexpedted report~n"
              "MessageId = ~p~n"
              "requestId = ~p~n"
              "seqNo = ~p~n",
              [MessageId,
               ocpp_message:get(<<"requestId">>, Message),
               ocpp_message:get(<<"seqNo">>, Message)]),
            keep_state_and_data
    end;
idle(cast, {rpccall, _, _, Message}, Data) ->
    NewData = handle_rpccall(Message, Data),
    {next_state, idle, NewData};
idle(cast, {error, Error}, #data{pending = MessageId} = Data) ->
    ErrorId = ocpp_error:id(Error),
    if
        MessageId =:= ErrorId ->
            send_error(Error, Data),
            {next_state, idle, clear_pending_request(Data)};
        MessageId =/= ErrorId ->
            keep_state_and_data
    end;
idle(cast, disconnect, Data) ->
    {next_state, offline, cleanup_connection(Data)};
idle(EventType, Event, Data) ->
    handle_event(EventType, Event, Data).

offline({call, From}, {connect, Pid}, Data) ->
    {next_state, reconnecting, setup_connection(Data, Pid),
     [{reply, From, ok}]};
offline({call, From}, {send_request, _, _}, _Data) ->
    {keep_state_and_data, [{reply, From, {error, offline}}]}.

reconnecting(cast, {rpccall, MessageType, _, _}, Data)
  when MessageType =:= 'Heartbeat';
       MessageType =:= 'StatusNotification' ->
    {next_state, provisioning, Data, [postpone]};
reconnecting(cast, {rpccall, MessageType, _, _}, Data)
  when MessageType =:= 'BootNotification' ->
    {next_state, connected, Data, [postpone]};
reconnecting({call, From}, {send_request, _, _}, _Data) ->
    {keep_state_and_data, [{reply, From, {error, offline}}]};
reconnecting(cast, disconnect, Data) ->
    {next_state, offline, cleanup_connection(Data)};
reconnecting(EventType, Event, Data) ->
    handle_event(EventType, Event, Data).

resetting(EventType, Event, Data) ->
    handle_event(EventType, Event, Data).

reset_accepted(EventType, Event, Data) ->
    handle_event(EventType, Event, Data).

reset_scheduled(EventType, Event, Data) ->
    handle_event(EventType, Event, Data).

code_change(_OldVsn, _NewVsn, Data) ->
    {ok, Data}.

terminate(_Reason, _State, _Data) ->
    ok.

handle_event({call, From},
             {lookup_variable, ComponentName, VariableName, VariableIdentifiers, AttributeType},
             #data{device_model = DeviceModel}) ->
    Reply =
        try ocpp_device_model:get_value(DeviceModel,
                                        ComponentName,
                                        VariableName,
                                        VariableIdentifiers,
                                        AttributeType)
        of
            undefined ->
                {error, undefined};
            Value ->
                {ok, Value}
        catch
            error:ambiguous ->
                {error, ambiguous};
            error:undefined ->
                {error, undefined}
        end,
    {keep_state_and_data, [{reply, From, Reply}]};
handle_event(cast, {rpcreply, MsgType, MessageId, Message},
     #data{pending_report = {MessageId, RequestId},
           pending_call = {TRef, MessageId}} = Data)
  when MsgType =:= 'GetReport';
       MsgType =:= 'GetBaseReport' ->
    timer:cancel(TRef),
    NewData =
        case ocpp_message:get(<<"status">>, Message) of
            <<"Accepted">> ->
                Data#data{pending_report = undefined,
                          expecting_report = [RequestId | Data#data.expecting_report],
                          pending_call = undefined};
            Status ->
                ocpp_handler:report_rejected(Data#data.stationid, RequestId, Status),
                Data#data{pending_report = undefined,
                          pending_call = undefined}
        end,
    {keep_state, NewData};
handle_event(cast, {rpcreply, _MsgType, MessageId, Message},
             #data{pending_call = {TRef, MessageId}} = Data) ->
    ocpp_handler:rpc_reply(Data#data.stationid, Message),
    timer:cancel(TRef),
    {keep_state, Data#data{pending_call = undefined,
                           pending_report = undefined}};
handle_event(cast, {rpcreply, _, _, _}, _) ->
    keep_state_and_data;
handle_event(info, {call_timeout, MessageId},
             #data{pending_call = {_TRef, MessageId}} = Data) ->
    %% TODO ocpp_handler:rpc_timeout(Data#data.stationid, MessageId),
    {keep_state, Data#data{pending_call = undefined}};
handle_event(info, {call_timeout, _}, _) ->
    keep_state_and_data;
handle_event(info, {'DOWN', Ref, process, Pid, _},
             #data{connection = {Pid, Ref}}) ->
    {keep_state_and_data, [{next_event, cast, disconnect}]}.

%%%%%% Internal functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_attribute(DeviceModel, ComponentName, VariableName, VariableIdentifiers, Properties) ->
    ocpp_device_model:add_attribute(
      DeviceModel,
      ComponentName,
      VariableName,
      VariableIdentifiers,
      binary_to_atom(proplists:get_value(type, Properties, <<"Actual">>)),
      proplists:get_value(value, Properties, undefined),
      proplists:get_value(mutability, Properties, [read, write]),
      proplists:get_value(persistent, Properties, false)).

add_attributes(DeviceModel, ComponentName, VariableName, VariableIdentifiers, Attributes) ->
    [true = add_attribute(DeviceModel,
                          binary_to_list(ComponentName),
                          binary_to_list(VariableName),
                          VariableIdentifiers,
                          Attribute)
     || Attribute <- Attributes].

boot_status_to_state(Message) ->
    case ocpp_message:get(<<"status">>, Message) of
        <<"Accepted">> -> provisioning;
        <<"Rejected">> -> connected;
        <<"Pending">>  -> boot_pending
    end.

call_station(Message, #data{pending_call = undefined} = Data, Timeout) ->
    ConnPid = connection_pid(Data),
    ConnPid ! {ocpp, {rpccall, Message}},
    MessageId = ocpp_message:id(Message),
    {ok, TRef} = timer:send_after(Timeout, self(), {call_timeout, MessageId}),
    NewData = Data#data{pending_call = {TRef, MessageId}},
    case ocpp_message:request_type(Message) of
        MessageType
          when MessageType =:= 'GetBaseReport';
               MessageType =:= 'GetReport' ->
            {ok, NewData#data{pending_report = {MessageId,
                                                ocpp_message:get(<<"requestId">>, Message)}}};
        _ ->
            {ok, NewData}
    end;
call_station(_Message, Data, _) ->
    {{error, busy}, Data}.

cleanup_connection(#data{connection = {_, Ref}} = Data) ->
    ocpp_handler:station_disconnected(Data#data.stationid),
    erlang:demonitor(Ref),
    Data#data{connection = disconnected}.

clear_pending_request(Data) ->
    Data#data{pending = undefined}.

connection_pid(#data{connection = {Pid, _Ref}}) -> Pid.

heartbeat_response(MessageId) ->
    ocpp_message:new_response(
      'Heartbeat',
      #{"currentTime" =>
            list_to_binary(
              calendar:system_time_to_rfc3339(
                erlang:system_time(second),
                [{offset, "Z"}, {unit, second}]))},
     MessageId).

handle_rpccall(Message, Data) ->
    ocpp_handler:rpc_request(Data#data.stationid, Message),
    Data#data{pending = ocpp_message:id(Message)}.

init_evse(EVSE) ->
    maps:from_list(lists:zip(lists:seq(1, length(EVSE)), EVSE)).

process_report(ReportMsg, Data) ->
    ReportData = ocpp_message:get(<<"reportData">>, ReportMsg, []),
    RequestId = ocpp_message:get(<<"requestId">>, ReportMsg),
    Seq = ocpp_message:get(<<"seqNo">>, ReportMsg),
    ToBeContinued = ocpp_message:get(<<"tbc">>, ReportMsg, false),
    logger:debug("Processing report part ~p, tbc=~p", [Seq, ToBeContinued]),
    lists:foreach(
      fun(RD) -> process_report_data(RD, Data#data.device_model) end, ReportData),
    if ToBeContinued ->
            Data;
       not ToBeContinued ->
            ocpp_handler:report_received(Data#data.stationid, RequestId),
            Data#data{expecting_report = lists:delete(RequestId, Data#data.expecting_report)}
    end.

process_report_data(ReportData, DeviceModel) ->
    ComponentName = ocpp_message:get(<<"component/name">>, ReportData),
    VariableName = ocpp_message:get(<<"variable/name">>, ReportData),
    Properties = report_variable_properties(ReportData),
    VariableIdentifiers =
        select_properties(
          Properties,
          [evse, connector, component_instance, variable_instance]),
    Attributes = proplists:get_value(attributes, Properties),
    true = ocpp_device_model:add_variable(
             DeviceModel,
             binary_to_list(ComponentName),
             binary_to_list(VariableName),
             VariableIdentifiers,
             maps:from_list(
               select_properties(Properties,
                                 [unit, datatype, minlimit, maxlimit,
                                  valuelist, supports_monitoring]))),
    add_attributes(DeviceModel, ComponentName, VariableName,
                   VariableIdentifiers, Attributes).

select_properties(Proplist, Keys) ->
    Xs = [proplists:lookup(K, Proplist) || K <- Keys],
    [X || X <- Xs, X =/= none].

send_error(Error, #data{connection = {ConnectionPid, _Ref}}) ->
    ConnectionPid ! {ocpp, {rpcerror, Error}},
    ok.

send_response(Response, #data{connection = {ConnectionPid, _Ref}}) ->
    ConnectionPid ! {ocpp, {rpcreply, Response}},
    ok.

setup_connection(Data, ConnectionPid) ->
    Ref = monitor(process, ConnectionPid),
    ocpp_handler:station_connected(Data#data.stationid),
    Data#data{connection = {ConnectionPid, Ref}}.

update_connector_status(EVSE, EVSEId, ConnectorId, Status) ->
    try
        NewEVSE =
            maps:update_with(
              EVSEId,
              fun (EVSEInfo) ->
                      ocpp_evse:set_status(EVSEInfo, ConnectorId, Status)
              end,
              EVSE),
        {ok, NewEVSE}
    catch error:badconnector ->
            {error, badconnector};
          error:{badkey, _} ->
            {error, badevse}
    end.

update_status(Message, Data) ->
    _Time = ocpp_message:get(<<"timestamp">>, Message),
    Status = ocpp_message:get(<<"connectorStatus">>, Message),
    EVSEId = ocpp_message:get(<<"evseId">>, Message),
    ConnectorId = ocpp_message:get(<<"connectorId">>, Message),
    case update_connector_status(Data#data.evse, EVSEId, ConnectorId, Status)
    of
        {ok, NewEVSE} ->
            {ok, Data#data{evse = NewEVSE}};
        {error, _Reason} ->
            {error, Data}
    end.

load_attribute(Attr) ->
    [{type, ocpp_message:get(<<"type">>, Attr, nil)},
     {value, ocpp_message:get(<<"value">>, Attr, nil)},
     {mutability, ocpp_message:get(<<"mutability">>, Attr, <<"ReadWrite">>)},
     {persistent, ocpp_message:get(<<"persistent">>, Attr, false)},
     {constant, ocpp_message:get(<<"constant">>, Attr, false)}].

report_attributes(Attrs) ->
    [load_attribute(Attr) || Attr <- Attrs].

report_variable_properties(ReportData) ->
    %% TODO construct a proplist with the optional properties
    %% contained in `ReportData'
    ComponentInstance = ocpp_message:get(<<"component/instance">>, ReportData, nil),
    EVSE = ocpp_message:get(<<"component/evse">>, ReportData, nil),
    {EVSEId, ConnectorId} =
        if EVSE =/= nil ->
                {ocpp_message:get(<<"id">>, EVSE),
                 ocpp_message:get(<<"connectorId">>, EVSE, nil)};
           EVSE =:= nil ->
                {nil, nil}
        end,
    VariableInstance = ocpp_message:get(<<"variable/instance">>, ReportData, nil),
    Attributes = report_attributes(ocpp_message:get(<<"variableAttribute">>, ReportData)),
    Unit = ocpp_message:get(<<"variableCharacteristics/unit">>, ReportData, nil),
    DataType = ocpp_message:get(<<"variableCharacteristics/dataType">>, ReportData),
    MinLimit = ocpp_message:get(<<"variableCharacteristics/minLimit">>, ReportData, nil),
    MaxLimit = ocpp_message:get(<<"variableCharacteristics/maxLimit">>, ReportData, nil),
    ValuesListCSV = ocpp_message:get(<<"variableCharacteristics/valuesList">>, ReportData, nil),
    ValuesList =
        if ValuesListCSV =/= nil ->
                [binary_to_list(Val) || Val <- binary:split(ValuesListCSV, <<",">>, [global])];
           ValuesListCSV =:= nil ->
                nil
        end,
    SuppportsMonitoring = ocpp_message:get(<<"variableCharacteristics/supportsMonitoring">>, ReportData),
    [if is_binary(Val) -> {Key, binary_to_list(Val)};
        true -> X
     end
     || {Key, Val} = X <- [{component_instance, ComponentInstance},
                           {evse, EVSEId},
                           {connector, ConnectorId},
                           {variable_instance, VariableInstance},
                           {attributes, Attributes},
                           {unit, Unit},
                           {datatype, if DataType =:= nil -> DataType;
                                         DataType =/= nil -> binary_to_atom(DataType)
                                      end},
                           {minlimit, MinLimit},
                           {maxlimit, MaxLimit},
                           {valuelist, ValuesList},
                           {supports_monitoring, SuppportsMonitoring}],
          Val =/= nil].
