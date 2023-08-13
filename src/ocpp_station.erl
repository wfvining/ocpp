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
         call_async/2, call_async/3,
         reply/2, rpcreply/2,
         error/2, %rpcerror/2,
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
         pending = undefined :: undefined
                              | {ocpp_message:messageid(), gen_statem:from()},
         reply_to = undefined :: undefined | {ocpp_message:messageid(), gen_statem:from()},
         pending_call = undefined :: undefined | {timer:tref(), ocpp_message:messageid()}}).

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
-spec rpccall(Station :: binary(), Request :: ocpp_message:message()) ->
          {ok, Response :: ocpp_message:message()} |
          {error, Reason :: ocpp_rpc:rpcerror()}.
rpccall(Station, Request) ->
    MessageType = ocpp_message:request_type(Request),
    MessageId = ocpp_message:id(Request),
    gen_statem:call(?registry(Station), {rpccall, MessageType, MessageId, Request}).

%% @doc Reply to an RPC call with a normal response.
-spec reply(StationId :: binary(), Response :: ocpp_message:message()) ->
          ok.
reply(StationId, Response) ->
    gen_statem:cast(?registry(StationId), {reply, Response}).

%% @equiv call(Station, Request, 30000)
-spec call(Station :: binary(), Request :: ocpp_message:message()) ->
          {ok, Response :: ocpp_message:message()} |
          {error, ocpp_error:error() | busy | offline}.
call(Station, Request) ->
    call(Station, Request, 30000).

-spec call(Station :: binary(), Request :: ocpp_message:message(),
           Timeout :: pos_integer()) ->
          {ok, Response :: ocpp_message:message()} |
          {error, ocpp_error:error() | busy | offline}.
call(Station, Request, Timeout) ->
    gen_statem:call(?registry(Station), {call, Request, Timeout}, Timeout + 5000).

%% @equiv call_async(Station, Request, 30000)
-spec call_async(Station :: binary(), Request :: ocpp_message:message()) ->
          ok |
          {error, Reason :: busy | offline}.
call_async(Station, Request) ->
    call_async(Station, Request, 30000).

%% @doc Send an RPCCALL message to the station. If there is an
%% outstanding request that has already been sent to the station this
%% will return `{error, busy}'. If the station is not connected
%% `{error, offline}' will be returned. The call will time out after
%% `Timeout' milliseconds.
%%
%% This is the asynchronous version of `call/3'. The calling process
%% returns immediately and the call response will be forwareded to the
%% station's handler process when it is received.
%% @end
-spec call_async(Station :: binary(), Request :: ocpp_message:message(),
           Timeout :: pos_integer()) ->
          ok |
          {error, Reason :: busy | offline}.
call_async(Station, Request, Timeout) ->
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
connected({call, From}, {rpccall, 'BootNotification', _, Message}, Data) ->
    NewData = handle_rpccall(Message, From , Data),
    {next_state, booting, NewData};
connected({call, From}, {rpccall, _, MessageId, _}, _Data) ->
    Error = ocpp_error:new('SecurityError', MessageId),
    {keep_state_and_data, [{reply, From, {error, Error}}]};
connected({call, From}, {send_request, _, _}, _Data) ->
    {keep_state_and_data, [{reply, From, {error, not_accepted}}]};
connected(EventType, Event, Data) ->
    handle_event(EventType, Event, Data).

booting(cast, disconnect, Data) ->
    {next_state, disconnected, cleanup_connection(Data)};
booting(cast, {reply, Response},
        #data{pending = {MessageId, From}} = Data) ->
    ResponseId = ocpp_message:id(Response),
    if
        MessageId =:= ResponseId ->
            NextState = boot_status_to_state(Response),
            {next_state, NextState, clear_pending_request(Data), [{reply, From, {ok, Response}}]};
        MessageId =/= ResponseId ->
            logger:notice(
              "Received out of order response to message ~p "
              "while awaiting response to message ~p.  "
              "Message dropped.",
              [ResponseId, MessageId]),
            keep_state_and_data
    end;
booting(cast, {error, Error},
        #data{pending = {MessageId, From}} = Data) ->
    ResponseId = ocpp_error:id(Error),
    if
        MessageId =:= ResponseId ->
            {next_state, connected, clear_pending_request(Data), [{reply, From, {error, Error}}]};
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
        'SetVariables' ->
            {Reply, NewData} = call_station(Message, Data, Timeout),
            {keep_state, NewData, [{reply, From, Reply}]};
        _ ->
            {keep_state_and_data, [{reply, From, {error, illegal_request}}]}
    end;
boot_pending(cast, disconnect, Data) ->
    {next_state, disconnected, cleanup_connection(Data)};
boot_pending({call, _From}, {rpccall, 'BootNotification', _, _}, Data) ->
    {next_state, connected, Data, [postpone]};
boot_pending({call, From}, {rpccall, _, _, Message}, _Data) ->
    Error =
        ocpp_error:new(
          'SecurityError',
          ocpp_message:id(Message),
          [{description,
            <<"The charging station is not allowed to initiate "
              "sending any messages other than a BootNotificationRequest "
              "before being accepted.">>}]),
    %% XXX It is not clear whether this is the correct next state.
    {keep_state_and_data, [{reply, From, {error, Error}}]};
boot_pending(EventType, Event, Data) ->
    handle_event(EventType, Event, Data).

provisioning({call, From}, {rpccall, 'Heartbeat', MessageId, _}, Data) ->
    {next_state, idle, Data, [{reply, From, {ok, heartbeat_response(MessageId)}}]};
provisioning({call, From}, {rpccall, 'StatusNotification', MessageId, Message}, Data) ->
    case update_status(Message, Data) of
        {ok, NewData} ->
            Response = {ok, ocpp_message:new_response(
                              'StatusNotification',
                              #{}, MessageId)},
            UpdatedData = NewData;
        {error, _} ->
            Response = {error, ocpp_error:new('GenericError', MessageId)},
            UpdatedData = Data
    end,
    {next_state, provisioning, UpdatedData, [{reply, From, Response}]};
provisioning(cast, disconnect, Data) ->
    %% The station has already been accepted here so transition is the
    %% same as for `idle'
    {next_state, offline, cleanup_connection(Data)};
provisioning(EventType, Event, Data) ->
    handle_event(EventType, Event, Data).

idle({call, From}, {call, Message, Timeout}, Data) ->
    case call_station(Message, Data, Timeout) of
        {ok, NewData} ->
            {keep_state, NewData#data{reply_to={ocpp_message:id(Message), From}}};
        {{error, _} = Error, NewData} ->
            {keep_state, NewData, [{reply, From, Error}]}
    end;
idle({call, From}, {send_request, Message, Timeout}, Data) ->
    {Reply, NewData} = call_station(Message, Data, Timeout),
    {keep_state, NewData, [{reply, From, Reply}]};
idle({call, From}, {rpccall, _, _, Message}, Data) ->
    NewData = handle_rpccall(Message, From, Data),
    {next_state, idle, NewData};
idle(cast, {error, Error}, #data{pending = {MessageId, From}} = Data) ->
    ErrorId = ocpp_error:id(Error),
    if
        MessageId =:= ErrorId ->
            {next_state, idle, clear_pending_request(Data), [{reply, From, {error, Error}}]};
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

reconnecting({call, _From}, {rpccall, MessageType, _, _}, Data)
  when MessageType =:= 'Heartbeat';
       MessageType =:= 'StatusNotification' ->
    {next_state, provisioning, Data, [postpone]};
reconnecting({call, _From}, {rpccall, MessageType, _, _}, Data)
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

handle_event(cast, {rpcreply, _MsgType, MessageId, Message},
             #data{pending_call = {TRef, MessageId},
                   reply_to = {MessageId, From}} = Data) ->
    timer:cancel(TRef),
    {keep_state, Data#data{pending_call = undefined, reply_to = undefined},
     [{reply, From, {ok, Message}}]};
handle_event(cast, {rpcreply, _MsgType, MessageId, Message},
             #data{pending_call = {TRef, MessageId}} = Data) ->
    ocpp_handler:rpc_reply(Data#data.stationid, Message),
    timer:cancel(TRef),
    {keep_state, Data#data{pending_call = undefined}};
handle_event(cast, {rpcreply, _, _, _}, _) ->
    keep_state_and_data;
handle_event(info, {call_timeout, MessageId},
             #data{pending_call = {_TRef, MessageId},
                   reply_to = {MessageId, From}} = Data) ->
    {keep_state, Data#data{pending_call = undefined, reply_to = undefined},
     [{reply, From, {error, timeout}}]};
handle_event(info, {call_timeout, MessageId},
             #data{pending_call = {_TRef, MessageId}} = Data) ->
    {keep_state, Data#data{pending_call = undefined}};
handle_event(info, {call_timeout, _}, _) ->
    keep_state_and_data;
handle_event(info, {'DOWN', Ref, process, Pid, _},
             #data{connection = {Pid, Ref}}) ->
    {keep_state_and_data, [{next_event, cast, disconnect}]}.

setup_connection(Data, ConnectionPid) ->
    Ref = monitor(process, ConnectionPid),
    ocpp_handler:station_connected(Data#data.stationid),
    Data#data{connection = {ConnectionPid, Ref}}.

cleanup_connection(#data{connection = {_, Ref}} = Data) ->
    ocpp_handler:station_disconnected(Data#data.stationid),
    erlang:demonitor(Ref),
    Data#data{connection = disconnected}.

connection_pid(#data{connection = {Pid, Ref}}) -> Pid.

handle_rpccall(Message, From, Data) ->
    ocpp_handler:rpc_request(Data#data.stationid, Message),
    Data#data{pending = {ocpp_message:id(Message), From}}.

clear_pending_request(Data) ->
    Data#data{pending = undefined}.

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

heartbeat_response(MessageId) ->
    ocpp_message:new_response(
      'Heartbeat',
      #{"currentTime" =>
            list_to_binary(
              calendar:system_time_to_rfc3339(
                erlang:system_time(second),
                [{offset, "Z"}, {unit, second}]))},
     MessageId).

init_evse(EVSE) ->
    maps:from_list(lists:zip(lists:seq(1, length(EVSE)), EVSE)).

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
    {ok, NewData};
call_station(_Message, Data, _) ->
    {{error, busy}, Data}.
