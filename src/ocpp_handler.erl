%%% @doc An OTP compliant behavior that is to be used by implementors
%%% of charging station management systems to connect their business
%%% logic with OCPP.
%%% @end
%%%
%%% Copyright (c) 2023 Will Vining <wfv@vining.dev>

-module(ocpp_handler).

-behaviour(gen_event).

-export([start_link/1, add_handler/3, add_handler/4, rpc_request/2]).
-export([init/1, handle_event/2, handle_call/2]).

-record(state, {handler_state :: any(),
                init_arg :: any(),
                mod :: module(),
                stationid :: binary()}).

-type request() :: ocpp_message:message().

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

%% @doc Initialize the handler state.
-callback init(InitArg :: any()) -> {ok, State :: any()} | {error, Reason :: any()}.

%% @doc Handle a BootNotificationRequest.
-callback boot_notification(Req :: request(), State :: any()) ->
    handler_ret(boot_response()).
-optional_callbacks([boot_notification/2]).

-define(registry(Name), {via, gproc, ?name(Name)}).
-define(name(Name), {n, l, {?MODULE, Name}}).

%%% ========= Public API =========

start_link(StationId) ->
    gen_event:start_link(?registry(StationId)).

%% @doc Install the CSMS handler module as the handler for OCPP
%% Messages.
-spec add_handler(StationId :: binary(),
                  CallbackModule :: module(),
                  InitArg :: any()) -> gen_event:add_handler_ret().
add_handler(StationId, CallbackModule, InitArg) ->
    gen_event:add_sup_handler(
      ?registry(StationId), ?MODULE, {StationId, CallbackModule, InitArg}).

%% @doc Re-install the handler after a crash.
-spec add_handler(StationId :: binary(),
                  CallbackModule :: module(),
                  InitArg :: any(),
                  Reason :: ocpp_error:error()) -> gen_event:add_handler_ret().
add_handler(StationId, CallbackModule, InitArg, Reason) ->
    gen_event:add_sup_handler(
      ?registry(StationId), ?MODULE,
      {recover, Reason, {StationId, CallbackModule, InitArg}}).

%% @doc Notify the event manager that an RPC Request has been received.
-spec rpc_request(StationId :: binary(), Request :: term()) -> ok.
rpc_request(StationId, Request) ->
    gen_event:notify(?registry(StationId), {rpc_request, Request}).

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
    case ocpp_message:type(Message) of
        <<"BootNotificationRequest">> ->
            NewState = do_request(boot_notification, Message, State),
            {ok, NewState}
    end,
    {ok, State}.

handle_call(Call, State) ->
    io:format("got call ~p while state is ~p~n", [Call, State]),
    {ok, error, State}.

%%% ========= Internal Functions =========

do_request(boot_notification, Message,
           #state{handler_state = HState, mod = Mod, stationid = StationId} = State) ->
    try Mod:boot_notification(Message, HState) of
        {reply, Response, NewHState} ->
            MessageId = ocpp_message:id(Message),
            ResponseMsg = ocpp_message:new(<<"BootNotificationResponse">>,
                                           keys_to_binary(Response),
                                           MessageId),
            ocpp_station:reply(StationId, ResponseMsg),
            State#state{ handler_state = NewHState};
        {error, Reason, NewHState} ->
            Error = ocpp_error:new(
                      ocpp_message:id(Message), 'InternalError',
                      [{details, #{<<"reason">> => Reason}}]),
            ocpp_station:error(StationId, Error),
            State#state{handler_state = NewHState}
    catch error:undef ->
            %% TODO reply with a NotSupported error.
            %% TODO handle errors from jerk here as well
            error('callback not implemented');
          Exception:Reason:Trace when Exception =:= error;
                                      Exception =:= exit ->
            logger:error("ocpp_handler ~p crashed.~n"
                         "StationId: ~p~n"
                         "Reason: ~p~n"
                         "Message: ~p~n"
                         "Handler State: ~p~n"
                         "Stack trace: ~p~n",
                         [Mod, StationId, Reason, Message, HState, Trace]),
            Error = ocpp_error:new(ocpp_message:id(Message), 'InternalError',
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
