%%% @doc representation of a charging station within the CSMS.
%%% @end
%%%
%%% Copyright (c) 2023 Will Vining <wfv@vining.dev>

-module(ocpp_station).

-behaviour(gen_statem).

-export([start_link/2, stop/1, rpc/2, reply/2, error/2, connect/1]).

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
                              | {ocpp_message:message_id(), gen_statem:from()}}).

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

%% @doc Handle an OCPP remote procedure call.
-spec rpc(Station :: binary(), Request :: ocpp_message:message()) ->
          {ok, Response :: ocpp_message:message()} |
          {error, Reason :: ocpp_rpc:rpcerror()}.
rpc(Station, Request) ->
   gen_statem:call(?registry(Station), {rpccall, Request}).

%% @doc Reply to an RPC call with a normal response.
-spec reply(StationId :: binary(), Response :: ocpp_message:message()) ->
          ok.
reply(StationId, Response) ->
    gen_statem:cast(?registry(StationId), {reply, Response}).

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
     [{reply, From, ok}]}.

connected({call, From}, {connect, _}, _Data) ->
    {keep_state_and_data, [{reply, From, {error, already_connected}}]};
connected(cast, disconnect, Data) ->
    {next_state, disconnected, cleanup_connection(Data)};
connected({call, From}, {rpccall, Message}, Data) ->
    NewData = handle_rpccall(Message, From , Data),
    {next_state, booting, NewData};
connected(EventType, Event, Data) ->
    handle_event(EventType, Event, Data).

booting(cast, disconnect, Data) ->
    {next_state, disconnected, cleanup_connection(Data)};
booting(cast, {reply, Response},
        #data{pending = {MessageId, From}} = Data) ->
    case ocpp_message:id(Response) of
        MessageId ->
            rpc_reply(From, Response),
            NextState =
                case ocpp_message:get(<<"status">>, Response) of
                    <<"Accepted">> -> provisioning;
                    <<"Rejected">> -> connected;
                    <<"Pending">>  -> boot_pending
                end,
            {next_state, NextState, clear_pending_request(Data)};
        ResponseMsgId ->
            logger:notice(
              "Received out of order response to message ~p "
              "while awaiting response to message ~p.  "
              "Message dropped.",
              [ResponseMsgId, MessageId]),
            keep_state_and_data
    end;
booting(cast, {error, Error},
        #data{pending = {MessageId, From}} = Data) ->
    case ocpp_error:id(Error) of
        MessageId ->
            rpc_error(From, Error),
            {next_state, connected, clear_pending_request(Data)};
        ResponseMsgId ->
            logger:notice(
              "Received out of order error for message ~p "
              "while awaiting response to message ~p.  "
              "Message dropped.",
              [ResponseMsgId, MessageId]),
            keep_state_and_data
    end;
booting(EventType, Event, Data) ->
    handle_event(EventType, Event, Data).

boot_pending(cast, disconnect, Data) ->
    {next_state, disconnected, cleanup_connection(Data)};
boot_pending({call, From}, {rpccall, Message}, Data) ->
    case ocpp_message:type(Message) of
        <<"BootNotificationRequest">> ->
            {next_state, connected, Data, [postpone]};
        _ ->
            rpc_error(
              From,
              ocpp_error:new(
                'SecurityError',
                ocpp_message:id(Message),
                [{description,
                  <<"The charging station is not allowed to initiate "
                    "sending any messages other than a BootNotificationRequest "
                    "before being accepted.">>}])),
            keep_state_and_data
    end;
boot_pending(EventType, Event, Data) ->
    handle_event(EventType, Event, Data).

provisioning({call, From}, {rpccall, Message}, Data) ->
    %% I don't really like using these case expressions instead of
    %% pattern matching in the function head. This is making me
    %% re-think the type used for the messages... Maybe atoms are
    %% fine... {rpccall, 'StatusNotification', PDU (jerk term hidden in `ocpp_message`)}
    case ocpp_message:type(Message) of
        <<"StatusNotificationRequest">> ->
            UpdatedData =
                case update_status(Message, Data) of
                    {ok, NewData} ->
                        gen_statem:reply(
                          From, {ok, ocpp_message:new_response(
                                       'StatusNotification',
                                       #{}, ocpp_message:id(Message))}),
                        NewData;
                    {error, _} ->
                        gen_statem:reply(
                          From, {error, ocpp_error:new('GenericError', ocpp_message:id(Message))}),
                        Data
                end,
            {next_state, provisioning, UpdatedData};
        <<"HeartbeatRequest">> ->
            gen_statem:reply(From, {ok, heartbeat_response(ocpp_message:id(Message))}),
            ocpp_handler:station_ready(Data#data.stationid),
            {next_state, idle, Data}
    end;
provisioning(cast, disconnect, Data) ->
    {next_state, disconnected, cleanup_connection(Data)};
provisioning(EventType, Event, Data) ->
    handle_event(EventType, Event, Data).

idle({call, From}, {rpccall, Message}, Data) ->
    NewData = handle_rpccall(Message, From, Data),
    {next_state, idle, NewData};
idle(cast, {error, Error}, #data{pending = {MessageId, From}} = Data) ->
    case ocpp_error:id(Error) of
        MessageId ->
            rpc_error(From, Error),
            {next_state, idle, clear_pending_request(Data)};
        _ ->
            keep_state_and_data
    end;
idle(cast, disconnect, Data) ->
    {next_state, offline, cleanup_connection(Data)};
idle(EventType, Event, Data) ->
    handle_event(EventType, Event, Data).

offline({call, From}, {connect, Pid}, Data) ->
    {next_state, reconnecting, setup_connection(Data, Pid),
     [{reply, From, ok}]};
offline(EventType, Event, Data) ->
    handle_event(EventType, Event, Data).

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

handle_rpccall(Message, From, Data) ->
    ocpp_handler:rpc_request(Data#data.stationid, Message),
    Data#data{pending = {ocpp_message:id(Message), From}}.

clear_pending_request(Data) ->
    Data#data{pending = undefined}.

rpc_reply(ReplyTo, Message) ->
    gen_statem:reply(ReplyTo, {ok, Message}).

rpc_error(ReplyTo, Error) ->
    gen_statem:reply(ReplyTo, {error, Error}).

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
