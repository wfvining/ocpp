%%% @doc State machine representing a charging station connected to
%%% the CSMS.
%%% @end
%%%
%%% Copyright (c) 2023 Will Vining <wfv@vining.dev>

-module(ocpp_station).

-behaviour(gen_statem).

-export([start_link/2, handle_rpc/2, stop/1]).
-export([accept/2]).
-export([init/1, callback_mode/0, code_change/3]).
%% State functions
-export([connected/3, booting/3]).

-export_type([station_ref/0]).

-opaque station_ref() :: pid() | reference().

-record(data, {station :: binary(),
               connection_pid :: undefined | pid(),
               pending_request :: undefined | gen_statem:from()}).

-spec start_link(Station :: binary(), ConnectionPid :: pid()) ->
          gen_statem:start_ret().
start_link(Station, ConnectionPid) ->
    gen_statem:start_link(?MODULE, {Station, ConnectionPid}, []).

%% @doc Handle an OCPP remote procedure call.
-spec handle_rpc(Station :: pid(), Request :: ocpp_request:request()) ->
          {reply, Response :: map()} |
          {error, Reason :: ocpp_rpc:rpcerror()}.
handle_rpc(Station, Request) ->
    gen_statem:call(Station, {rpccall, Request}).

%% @doc Notify the station that it has been accepted by the CSMS.
-spec accept(Station :: station_ref(), Interval :: pos_integer()) -> ok.
accept(Station, Interval) ->
    gen_statem:cast(Station, {accept, Interval}).

-spec stop(StationPid :: pid()) -> ok.
stop(StationPid) ->
    gen_statem:stop(StationPid).

callback_mode() -> state_functions.

init({Station, ConnectionPid}) ->
    link(ConnectionPid), %% May want this to be a monitor, but I just don't want to deal rn
    {ok, connected, #data{station = Station,
                          connection_pid = ConnectionPid}}.

connected({call, From},
          {rpccall, {boot_notification, RPCMessage}},
          Data) ->
    ocpp_csms:boot_request(Data#data.station, RPCMessage),
    {next_state, booting, Data#data{pending_request = From}}.

booting(cast, {accepted, Interval}, Data) ->
    BootNotificationResponse = ocpp_response:boot_notification(
                                 accepted,
                                 Interval,
                                 calendar:universal_time()),
    gen_statem:reply(Data#data.pending_request, BootNotificationResponse),
    {next_state, idle, Data#data{pending_request = undefined}};
booting(cast, {rejected, RequestRef, Interval}, Data) ->
    BootNotificationResponse = ocpp_response:boot_notification(
                                 rejected,
                                 Interval,
                                 calendar:universal_time()),
    gen_statem:reply(Data#data.pending_request, BootNotificationResponse),
    {next_state, connected, Data#data{pending_request = undeinfed}}.

code_change(_OldVsn, _NewVsn, Data) ->
    {ok, Data}.
