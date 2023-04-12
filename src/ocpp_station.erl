%%% @doc representation of a charging station within the CSMS.
%%% @end
%%%
%%% Copyright (c) 2023 Will Vining <wfv@vining.dev>

-module(ocpp_station).

-behaviour(gen_statem).

-export([start_link/1, stop/1, handle_rpc/2, connect/2]).

-export([init/1, callback_mode/0, code_change/3]).

-define(NAME(StationId), {via, ocpp_station_registry, StationId}).

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
         connection = disconnected :: disconnected | {pid(), reference()}}).

-spec start_link(Station :: binary()) -> gen_statem:start_ret().
start_link(Station) ->
    gen_statem:start_link(?NAME(Station), ?MODULE, Station, []).

%% @doc Notify the state machine that a station has connected.
-spec connect(StationId :: binary(), ConnectionPid :: pid())
             -> ok | {error, already_connected}.
connect(StationId, ConnectionPid) ->
    gen_statem:call(?NAME(StationId), {connect, ConnectionPid}).

%% @doc Handle an OCPP remote procedure call.
-spec handle_rpc(StationId :: binary(), Request :: ocpp_request:request()) ->
          {reply, Response :: map()} |
          {error, Reason :: ocpp_rpc:rpcerror()}.
handle_rpc(StationId, Request) ->
    gen_statem:call(?NAME(StationId), {rpccall, Request}).

-spec stop(StationId :: binary()) -> ok.
stop(StationId) ->
    gen_statem:stop(?NAME(StationId)).

callback_mode() -> state_functions.

init(StationId) ->
    {ok, disconnected, #data{stationid = StationId}}.

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

handle_event(info, {'DOWN', Ref, process, Pid, _},
             #data{connection = {Pid, Ref}}) ->
    {keep_state_and_data, [{next_event, cast, disconnect}]}.

setup_connection(Data, ConnectionPid) ->
    Ref = monitor(process, ConnectionPid),
    Data#data{connection = {ConnectionPid, Ref}}.

cleanup_connection(Data) ->
    Data#data{connection = disconnected}.
