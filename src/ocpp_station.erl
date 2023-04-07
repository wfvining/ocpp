%%% @doc representation of a charging station within the CSMS.
%%% @end
%%%
%%% Copyright (c) 2023 Will Vining <wfv@vining.dev>

-module(ocpp_station).

-behaviour(gen_statem).

-export([start_link/1, handle_rpc/2, stop/1, connect/2]).
-export([init/1, callback_mode/0, code_change/3]).
%% State functions
-export([disconnected/3,
         connected/3,
         booting/3]).

-define(ALL_STATE_EVENT,
        ?FUNCTION_NAME(EventType, Event, Data) ->
               handle_event(EventType, Event, ?FUNCTION_NAME, Data)).

%% -define(UNKNOWN_EVENT(EventType, Event, Data),
%%         handle_event(EventType, Event, ?FUNCTION_NAME, Data)).

-record(data, {station :: binary(),
               connection_pid :: undefined | pid(),
               connection_ref :: undefined | reference(),
               pending_request :: undefined | gen_statem:from(),
               csms_request :: undefined | reference()}).

-spec start_link(Station :: binary()) -> gen_statem:start_ret().
start_link(Station) ->
    gen_statem:start_link(?MODULE, Station, []).

%% @doc Notify the state machine that a station has connected.
-spec connect(StationPid :: pid(), ConnectionPid :: pid())
             -> ok
              | {error, already_connected}. %% TODO what are the other possible errors.
connect(StationPid, ConnectionPid) ->
    gen_statem:call(StationPid, {connect, ConnectionPid}).

%% @doc Handle an OCPP remote procedure call.
-spec handle_rpc(Station :: pid(), Request :: ocpp_request:request()) ->
          {reply, Response :: map()} |
          {error, Reason :: ocpp_rpc:rpcerror()}.
handle_rpc(Station, Request) ->
    gen_statem:call(Station, {rpccall, Request}).

-spec stop(StationPid :: pid()) -> ok.
stop(StationPid) ->
    gen_statem:stop(StationPid).

callback_mode() -> state_functions.

init(Station) ->
    {ok, disconnected, #data{station = Station}}.

disconnected(cast, {connect, ConnectionPid}, Data) ->
    Ref = erlang:monitor(process, ConnectionPid),
    {next_state, connected,
     Data#data{connection_ref = Ref, connection_pid = ConnectionPid}}.

connected({call, From},
          {rpccall, {boot_notification, _}} = RPCMessage,
          Data) ->
    CSMSRequestRef = ocpp_csms:boot_request(RPCMessage),
    %% no reply, instead we change state and wait for
    %% instruction from the CSMS.
    %%
    %% QUESTION: How do we handle a timeout? - we should get some kind
    %% of notification based on how the client responds. For now,
    %% assume that we will get an 'EXIT' notification via the monitor.
    %% This approach means we still need to handle delayed/out of order
    %% messages from the CSMS.
    {next_state, booting, Data#data{pending_request = From,
                                    csms_request = CSMSRequestRef}};
?ALL_STATE_EVENT.

booting(cast, {accepted, RequestRef, Interval},
        #data{csms_request = RequestRef} = Data) ->
    BootNotificationResponse = ocpp_response:boot_notification(
                                 accepted,
                                 Interval,
                                 calendar:universal_time()),
    gen_statem:reply(Data#data.pending_request, BootNotificationResponse),
    {next_state, idle, Data#data{pending_request = undefined,
                                 csms_request = undefined}};
booting(cast, {rejected, RequestRef, Interval},
        #data{csms_request = RequestRef} = Data) ->
    BootNotificationResponse = ocpp_response:boot_notification(
                                 rejected,
                                 Interval,
                                 calendar:universal_time()),
    gen_statem:reply(Data#data.pending_request, BootNotificationResponse),
    {next_state, connected, Data#data{pending_request = undeinfed}};
?ALL_STATE_EVENT.

code_change(_OldVsn, _NewVsn, Data) ->
    {ok, Data}.

handle_event(call, {connect, Pid}, State, Data) when State =/= disconnected ->
    %% ocpp_event:security(duplicate_connection, Pid, Data),
    logger:warning(
      "Received duplicate connection from station ~p (~p) in state ~p",
      [Data#data.station, Pid, State]),
    {keep_state_and_data, {reply, {error, already_connected}}};
handle_event(info, {'EXIT', Ref, process, Pid, Reason},
             _State,
             Data = #data{connection_pid = Pid,
                          connection_ref = Ref}) ->
    logger:debug(
      "Connection process ~p exited. Reason: ~p", [Pid, Reason]),
    {next_state, disconnected, cleanup_connection(Data)}.

cleanup_connection(#data{csms_request = undefined} = Data) ->
    Data#data{connection_pid = undefined,
              connection_ref = undefined,
              pending_request = undefined};
cleanup_connection(#data{csms_request = CSMSRequestRef} = Data) ->
    erlang:demonitor(Data#data.connection_ref),
    ocpp_csms:cancel_request(CSMSRequestRef),
    Data#data{connection_pid = undefined,
              connection_ref = undefined,
              pending_request = undefined,
              csms_request = undefined}.
