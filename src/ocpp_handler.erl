%%% @doc An OTP compliant behavior that is to be used by implementers
%%% of charging station management systems to connect their business
%%% logic with OCPP.
%%%
%%% The only required callback is `init/1'. All others are
%%% optional. If `handle_info/2' is not implemented, info messages are
%%% dropped. Any OCPP message type for which the corresponding
%%% callback is unimplemented will receive an error response with the
%%% error code `"NotSupported"'.
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
-export([rpc_request/2, rpc_reply/2]).
%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2]).

-export_type([boot_status/0, boot_response/0, status_info/0]).
-export_type([handler_error/0, handler_ret/1]).

-record(state, {handler_state :: any(),
                init_arg :: any(),
                mod :: module(),
                stationid :: binary()}).

-type request() :: ocpp_message:message().
-type response() :: ocpp_message:message().

%%% ========= OCPP Message-Related Types =========

-type boot_status() :: 'Accepted' | 'Rejected' | 'Pending'.

-type status_info() :: #{'reasonCode' := binary(),
                         'additionalInfo' => binary()}.

-type boot_response() :: #{'status' := boot_status(),
                           'interval' := non_neg_integer(),
                           'currentTime' := calendar:datetime(),
                           'statusInfo' => status_info()}.

-type handler_error() :: #{'ErrorCode' => binary(),
                           'ErrorDescription' => binary(),
                           'ErrorDetails' => #{binary() => any()}}.

-type handler_ret(ResponseType) :: {reply, ResponseType, any()}
                                 | {error, handler_error(), any()}.

%%% ========= Callback Definitions =========

-callback init(InitArg :: any()) -> {ok, State :: any()} | {error, Reason :: any()}.
%% Initialize any internal state the handler will need when responding
%% to requests.

-callback handle_call_response(Response :: response(),
                               State :: any()) ->
    {ok, NewState :: any()}.
%% Handle a response to an RPCCALL made to the station.

-callback handle_info(any(), State :: any()) -> {ok, NewState :: any()}.
%% Handle events that are not OCPP messages.

-callback boot_notification(Req :: request(), State :: any()) ->
    handler_ret(boot_response()).
%% Handle a BootNotificationRequest.

-optional_callbacks([boot_notification/2, handle_info/2]).

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

%% @doc Notify the event manager that an RPC Request has been received.
%%
%% This function is used internally and should not be called directly.
-spec rpc_request(StationId :: binary(), Request :: term()) -> ok.
rpc_request(StationId, Request) ->
    gen_event:notify(?registry(StationId), {rpc_request, Request}).

-spec rpc_reply(StationId :: binary(), Reply :: ocpp_message:message()) -> ok.
rpc_reply(StationId, Reply) ->
    gen_event:notify(?registry(StationId), {rpc_reply, Reply}).

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

handle_event({rpc_request, Message}, State) ->
    RequestFun = request_fun(ocpp_message:type(Message)),
    NewState = do_request(RequestFun, Message, State),
    {ok, NewState};
handle_event({rpc_reply, Message}, #state{mod = Mod, handler_state = HState} = State) ->
    {ok, NewHState} = Mod:handle_call_response(Message, HState),
    {ok, State#state{handler_state = NewHState}};
handle_event(Event, #state{mod = Mod, handler_state = HState} = State) ->
    try Mod:handle_info(Event, HState) of
        {ok, NewState} ->
            {ok, State#state{ handler_state = NewState }}
    catch
        error:undef ->
            {ok, State}
    end.

request_fun(<<"BootNotificationRequest">>) -> boot_notification;
request_fun(<<"Get15118EVCertificateRequest">>) -> get_15118_ev_certificate.

response_type(boot_notification) -> 'BootNotification';
response_type(get_15118_ev_certificate) -> 'Get1511EVCertificate'.

handle_call(Call, State) ->
    io:format("got call ~p while state is ~p~n", [Call, State]),
    {ok, error, State}.

%%% ========= Internal Functions =========

do_request(RequestFun, Message,
           #state{handler_state = HState, mod = Mod, stationid = StationId} = State) ->
    try Mod:RequestFun(Message, HState) of
        {reply, Response, NewHState} ->
            MessageId = ocpp_message:id(Message),
            ResponseMsg = ocpp_message:new_response(
                            response_type(RequestFun),
                            keys_to_binary(Response),
                            MessageId),
            ocpp_station:reply(StationId, ResponseMsg),
            State#state{ handler_state = NewHState};
        {error, Reason, NewHState} ->
            Error = ocpp_error:new(
                      'InternalError',
                      ocpp_message:id(Message),
                      [{details, #{<<"reason">> => Reason}}]),
            ocpp_station:error(StationId, Error),
            State#state{handler_state = NewHState}
    catch error:undef ->
            ocpp_station:error(
              StationId,
              ocpp_error:new('NotSupported', ocpp_message:id(Message))),
            State;
          Exception:Reason:Trace when Exception =:= error;
                                      Exception =:= exit ->
            logger:error("ocpp_handler ~p crashed.~n"
                         "StationId: ~p~n"
                         "Reason: ~p~n"
                         "Message: ~p~n"
                         "Handler State: ~p~n"
                         "Stack trace: ~p~n",
                         [Mod, StationId, Reason, Message, HState, Trace]),
            Error = ocpp_error:new('InternalError',
                                   ocpp_message:id(Message),
                                   [{details, #{<<"reason">> => Reason}}]),
            error({ocpp_handler_error, Error})
    end.

keys_to_binary(Map) ->
    maps:fold(
      fun (Key, Value, Acc) ->
              Val = if
                        is_map(Value) -> keys_to_binary(Value);
                        true -> Value
                    end,
              Acc#{atom_to_binary(Key) => Val}
      end, #{}, Map).
