%%% @doc representation of a charging station within the CSMS.
%%% @end
%%%
%%% Copyright (c) 2023 Will Vining <wfv@vining.dev>

-module(ocpp_station).

-behaviour(gen_statem).

-export([start_link/2, stop/1, handle_rpc/2, connect/2, lookup/1]).

-export([init/1, callback_mode/0, code_change/3, terminate/3]).

%% State functions
-export([disconnected/3,
         connected/3,
         booting/3,
         boot_pending/3,
         idle/3,
         offline/3,
         reconnecting/3,
         resetting/3,
         reset_accepted/3,
         reset_scheduled/3]).

-record(data,
        {stationid :: binary(),
         connection = disconnected :: disconnected | {pid(), reference()},
         evse_sup :: pid(),
         evse :: [pid()]}).

-include_lib("eunit/include/eunit.hrl").

-spec start_link(Station :: binary(), NumEVSE :: pos_integer()) -> gen_statem:start_ret().
start_link(Station, NumEVSE) ->
    gen_statem:start_link(?MODULE, {Station, NumEVSE, self()}, []).

%% @doc Notify the state machine that a station has connected.
-spec connect(Station :: pid(), ConnectionPid :: pid())
             -> ok | {error, already_connected}.
connect(Station, ConnectionPid) ->
    gen_statem:call(Station, {connect, ConnectionPid}).

%% @doc Handle an OCPP remote procedure call.
-spec handle_rpc(Station :: pid(), Request :: ocpp_request:request()) ->
          {reply, Response :: map()} |
          {error, Reason :: ocpp_rpc:rpcerror()}.
handle_rpc(Station, Request) ->
   gen_statem:call(Station, {rpccall, Request}).

-spec stop(Station :: pid()) -> ok.
stop(Station) ->
    gen_statem:stop(Station).

%% @doc
%% Lookup the Pid corresponding to the `StationId'. If `StationId' is
%% not a station that is currently running then the call fails with
%% reason `badarg'.
%% @end
-spec lookup(StationId :: binary()) -> pid().
lookup(StationId) ->
    case ocpp_station_registry:lookup_station(StationId) of
        {ok, Pid} ->
            Pid;
        {error, unregistered} ->
            error(badarg, [StationId])
    end.


callback_mode() -> state_functions.

init({StationId, NumEVSE, Sup}) ->
    process_flag(trap_exit, true),
    case ocpp_station_registry:register(StationId) of
        ok ->
            {ok, disconnected, #data{stationid = StationId},
             [{next_event, internal, {init_evse, Sup, NumEVSE}}]};
        {error, already_registered} ->
            {stop, {already_registered,
                    element(2, ocpp_station_registry:lookup_station(StationId))}}
    end.

disconnected(internal, {init_evse, Sup, NumEVSE}, Data) ->
    {ok, EVSESup} = ocpp_station_sup:start_evse_sup(Sup),
    EVSE = init_evse(EVSESup, NumEVSE),
    {keep_state, Data#data{evse_sup = EVSESup, evse = EVSE}};
disconnected({call, From}, {connect, ConnectionPid}, Data) ->
    {next_state, connected,
     setup_connection(Data, ConnectionPid),
     [{reply, From, ok}]}.

connected({call, From}, {connect, _}, _Data) ->
    {keep_state_and_data, [{reply, From, {error, already_connected}}]};
connected(cast, disconnect, Data) ->
    {next_state, disconnected, cleanup_connection(Data)};
connected(EventType, Event, Data) ->
    handle_event(EventType, Event, Data).

booting(_, _, _) ->
    error('not implemented').

boot_pending(_, _, _) ->
    error('not implemented').

idle(_, _, _) ->
    error('not implemented').

offline(_, _, _) ->
    error('not implemented').

reconnecting(_, _, _) ->
    error('not implemented').

resetting(_, _, _) ->
    error('not implemented').

reset_accepted(_, _, _) ->
    error('not implemented').

reset_scheduled(_, _, _) ->
    error('not implemented').

code_change(_OldVsn, _NewVsn, Data) ->
    {ok, Data}.

terminate(_Reason, _State, _Data) ->
    ocpp_station_registry:unregister().

handle_event(info, {'DOWN', Ref, process, Pid, _},
             #data{connection = {Pid, Ref}}) ->
    {keep_state_and_data, [{next_event, cast, disconnect}]}.

setup_connection(Data, ConnectionPid) ->
    Ref = monitor(process, ConnectionPid),
    Data#data{connection = {ConnectionPid, Ref}}.

cleanup_connection(Data) ->
    Data#data{connection = disconnected}.

init_evse(_, 0) ->
    [];
init_evse(EVSESup, N) ->
    [ocpp_evse_sup:start_evse(EVSESup, N)|init_evse(EVSESup, N - 1)].
