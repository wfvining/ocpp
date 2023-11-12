%%% @doc An OTP compliant behavior that is to be used by implementers
%%% of charging station management systems to connect their business
%%% logic with OCPP.
%%%
%%% The required callbacks are `init/1', and `handle_ocpp/3'. The
%%% latter is called whenever an OCPP request that requires the CSMS
%%% to reply is received from the station. Messages such as heartbeats
%%% are handled automatically and don't require CSMS intervention,
%%% therefor this callback is not invoked for such
%%% messages. Similarly, messages that may arrive in several parts
%%% such as Report Notifications will not trigger this
%%% callback. Instead, once the full report is received a
%%% `{report_received, RequestId}' event will be passed to the
%%% `handle_info/2' callback.
%%%
%%% If your CSMS implementation uses asynchronous calls to the station
%%% you should implement the `handle_async_reply/3' callback. This
%%% callback is invoked whenever the response to an asychronous call
%%% is received. If it is not implemented the response will be dropped.
%%%
%%% <b>Note:</b> All exported functions from this module are used
%%% internally by the ocpp application and should not be called directly.
%%% @end
%%%
%%% Copyright (c) 2023 Will Vining <wfv@vining.dev>

-module(ocpp_handler).

-behaviour(gen_event).

%% management API
-export([start_link/1, add_handler/3, add_handler/4]).
%% informational events
-export([station_connected/1,
         station_disconnected/1,
         station_ready/1]).
%% OCPP events
-export([rpc_request/2, rpc_reply/2, rpc_error/2, report_received/2, reboot_required/1]).
%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2]).

-record(state, {handler_state :: any(),
                init_arg :: any(),
                mod :: module(),
                stationid :: binary()}).

%%% ========= Callback Definitions =========

-callback init(InitArg :: any()) -> {ok, State :: any()} | {error, Reason :: any()}.
%% Initialize any internal state the handler will need when responding
%% to requests.

-callback handle_ocpp(MsgType :: ocpp_message:messagetype(),
                      Message :: ocpp_message:message(),
                      State :: any()) ->
    {reply, Message :: ocpp_message:message(), NewState :: any()} |
    {error, Reason :: ocpp_error:error(), NewState :: any()} |
    {noreply, NewState :: any()}.
%% Handle a message sent by the charging station to the CSMS.

-callback handle_info(Info, State :: any()) ->
    {ok, NewState :: any()}
        when Info :: reboot_required |
                     {report_received, ReportId :: integer()} |
                     station_connected |
                     station_disconnected |
                     station_ready.
%% Handle an event other than receipt of an OCPP message.

-callback handle_async_reply(ReplyType, Message, State :: any()) ->
    {ok, NewState :: any()}
        when Message :: ocpp_message:message() | ocpp_error:error(),
             ReplyType :: callresult | callerror.
%% Called when the response to an asynchronous RPCCALL is received.

-optional_callbacks([handle_async_reply/3]).

-define(registry(Name), {via, gproc, ?name(Name)}).
-define(name(Name), {n, l, {?MODULE, Name}}).

%%% ========= Public API =========

start_link(StationId) ->
    gen_event:start_link(?registry(StationId)).

%% @doc Install `CallbackModule' as the handler for OCPP Messages.
%%
%% This function is used internally and should not be called directly.
-spec add_handler(StationId :: binary(),
                  CallbackModule :: module(),
                  InitArg :: any()) -> gen_event:add_handler_ret().
add_handler(StationId, CallbackModule, InitArg) ->
    gen_event:add_sup_handler(
      ?registry(StationId), ?MODULE, {StationId, CallbackModule, InitArg}).

%% @doc Re-install the handler after a crash.
%%
%% This function is used internally and should not be called directly.
-spec add_handler(StationId :: binary(),
                  CallbackModule :: module(),
                  InitArg :: any(),
                  Reason :: ocpp_error:error()) -> gen_event:add_handler_ret().
add_handler(StationId, CallbackModule, InitArg, Reason) ->
    gen_event:add_sup_handler(
      ?registry(StationId), ?MODULE,
      {recover, Reason, {StationId, CallbackModule, InitArg}}).

%% @doc Notify the event manager that the station has indicated it
%% needs to be rebooted.
-spec reboot_required(StationId :: binary()) -> ok.
reboot_required(StationId) ->
    gen_event:notify(?registry(StationId), reboot_required).

-spec report_received(StationId :: binary(), RequestId :: integer()) -> ok.
report_received(StationId, RequestId) ->
    gen_event:notify(?registry(StationId), {report_received, RequestId}).

%% @doc Notify the event manager that an RPC Request has been received.
%%
%% This function is used internally and should not be called directly.
-spec rpc_request(StationId :: binary(), Request :: term()) -> ok.
rpc_request(StationId, Request) ->
    gen_event:notify(?registry(StationId), {rpc_request, Request}).

-spec rpc_reply(StationId :: binary(), Reply :: ocpp_message:message()) -> ok.
rpc_reply(StationId, Reply) ->
    gen_event:notify(?registry(StationId), {rpc_reply, Reply}).

-spec rpc_error(StationId :: binary(), Error :: ocpp_error:error()) -> ok.
rpc_error(StationId, Error) ->
    gen_event:notify(?registry(StationId), {rpc_error, Error}).

-spec station_connected(StationId :: binary()) -> ok.
station_connected(StationId) ->
    gen_event:notify(?registry(StationId), station_connected).

-spec station_disconnected(StationId :: binary()) -> ok.
station_disconnected(StationId) ->
    gen_event:notify(?registry(StationId), station_disconnected).

%% @doc The station has fully booted and is idle.
-spec station_ready(StationId :: binary()) -> ok.
station_ready(StationId) ->
    gen_event:notify(?registry(StationId), station_ready).

%%% ========= gen_event callbacks =========

init({recover, Reason, {StationId, _, _} = InitArg}) ->
    ocpp_station:error(StationId, Reason),
    init(InitArg);
init({StationId, CallbackModule, InitArg}) ->
    try CallbackModule:init(InitArg) of
        {ok, State} ->
            {ok, #state{handler_state = State,
                        init_arg = InitArg,
                        mod = CallbackModule,
                        stationid = StationId}};
        {error, _} = Error ->
            Error
    catch error:Reason ->
            {error, {init, Reason}}
    end.

handle_event({rpc_request, Message}, #state{stationid = StationId, mod = Mod, handler_state = HState} = State) ->
    case Mod:handle_ocpp(ocpp_message:request_type(Message), Message, HState) of
        {reply, Response, NewHState} ->
            ocpp_station:reply(StationId, Response),
            {ok, State#state{handler_state = NewHState}};
        {error, Error, NewHState} ->
            ocpp_station:error(StationId, Error),
            {ok, State#state{handler_state = NewHState}};
        {noreply, NewHState} ->
            {ok, State#state{handler_state = NewHState}};
        Ret ->
            error({bad_return, Ret})
    end;
handle_event({rpc_reply, Message}, #state{mod = Mod, handler_state = HState} = State) ->
    try
        {ok, NewHState} = Mod:handle_async_reply(ocpp_message:response_type(Message), Message, HState),
        {ok, State#state{handler_state = NewHState}}
    catch error:undef ->
            {ok, State}
    end;
handle_event(Event, #state{mod = Mod, handler_state = HState} = State) ->
    try Mod:handle_info(Event, HState) of
        {ok, NewState} ->
            {ok, State#state{ handler_state = NewState }}
    catch
        error:undef ->
            {ok, State}
    end.

handle_call(Call, State) ->
    io:format("got call ~p while state is ~p~n", [Call, State]),
    {ok, error, State}.
