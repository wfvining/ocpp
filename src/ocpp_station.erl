%%% @doc representation of a charging station within the CSMS.
%%% @end
%%%
%%% Copyright (c) 2023 Will Vining <wfv@vining.dev>

-module(ocpp_station).

-behaviour(gen_statem).

-export([start_link/3, stop/1, rpc/2, reply/2, connect/1]).

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

-define(registry(Module, Name), {via, gproc, {n, l, {Module, Name}}}).
-define(registry(Name), ?registry(?MODULE, Name)).

-record(data,
        {stationid :: binary(),
         connection = disconnected :: disconnected | {pid(), reference()},
         evse_sup :: undefined | pid(),
         evse = [] :: [pid()],
         pending = undefined :: undefined
                              | {ocpp_message:message_id(), gen_statem:from()}}).

-spec start_link(StationId :: binary(),
                 NumEVSE :: pos_integer(),
                 Supervisor :: pid()) -> gen_statem:start_ret().
start_link(StationId, NumEVSE, Supervisor) ->
    gen_statem:start_link(
      ?registry(StationId), ?MODULE, {StationId, NumEVSE, Supervisor}, []).

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

-spec reply(StationId :: binary(), Response :: ocpp_message:message()) ->
          ok.
reply(StationId, Response) ->
    gen_statem:cast(?registry(StationId), {reply, Response}).

-spec stop(Station :: pid()) -> ok.
stop(Station) ->
    gen_statem:stop(Station).

callback_mode() -> state_functions.

init({StationId, NumEVSE, Sup}) ->
    process_flag(trap_exit, true),
    {ok, disconnected, #data{stationid = StationId},
     [{next_event, internal, {init_evse, Sup, NumEVSE}}]}.

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
connected({call, From}, {rpccall, Message}, Data) ->
    NewData = Data#data{pending = {ocpp_message:id(Message), From}},
    ocpp_handler:rpc_request(Data#data.stationid, Message),
    {next_state, booting, NewData};
connected(EventType, Event, Data) ->
    handle_event(EventType, Event, Data).

booting(cast, {reply, Response},
        #data{pending = {MessageId, From}} = Data) ->
    case ocpp_message:id(Response) of
        MessageId ->
            rpc_reply(From, Response),
            NextState =
                case ocpp_message:get(<<"status">>, Response) of
                    <<"Accepted">> -> idle;
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
booting(EventType, Event, Data) ->
    handle_event(EventType, Event, Data).

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
    ok.

handle_event(info, {'DOWN', Ref, process, Pid, _},
             #data{connection = {Pid, Ref}}) ->
    {keep_state_and_data, [{next_event, cast, disconnect}]}.

setup_connection(Data, ConnectionPid) ->
    Ref = monitor(process, ConnectionPid),
    Data#data{connection = {ConnectionPid, Ref}}.

cleanup_connection(Data) ->
    Data#data{connection = disconnected}.

clear_pending_request(Data) ->
    Data#data{pending = undefined}.

init_evse(_, 0) ->
    [];
init_evse(EVSESup, N) ->
    [ocpp_evse_sup:start_evse(EVSESup, N)|init_evse(EVSESup, N - 1)].

rpc_reply(ReplyTo, Message) ->
    gen_server:reply(ReplyTo, {ok, Message}).
