%%% @doc An OTP compliant behavior that is to be used by implementors
%%% of charging station management systems to connect their business
%%% logic with OCPP.
%%% @end
%%%
%%% Copyright (c) 2023 Will Vining <wfv@vining.dev>

-module(ocpp_handler).

-behaviour(gen_event).

-export([start_link/1, add_handler/3, rpc_request/2, rpc_response/2]).
-export([init/1, handle_event/2, handle_call/2]).

-record(state, {handler_state :: any(), handler :: {module(), any()}}).

-type request() :: jerk:term().  % TODO
-type boot_status() :: accepted | pending | rejected.

%% @doc Either a string with the reasonCode (up to 20 characters) or a
%% reasonCode and another string containing additional info (up to 512
%% characters).
-type status_info() :: string()
                     | {string(), string()}.

%% @doc Initialize the handler state.
-callback init(InitArg :: any()) -> {ok, State :: any()} | {error, Reason :: any()}.

%% @doc Handle a BootNotificationRequest if the required OCPP options
%% `interval' and `current_time' are not specified they will be
%% set to `0' and the current time in UTC respectively.
-callback boot_notification(request()) ->
    {boot_status(), [Option]}
        when Option :: {interval, non_neg_integer()}
                     | {current_time, calendar:datetime()}
                     | {status_info, status_info()}.

-optional_callbacks([boot_notification/1]).

-define(registry(Name), {via, gproc, ?name(Name)}).
-define(name(Name), {n, l, {?MODULE, Name}}).

start_link(StationId) ->
    gen_event:start_link(?registry(StationId)).

%% @doc Install the CSMS handler module as the handler for OCPP
%% Messages.
-spec add_handler(Manager :: pid(),
                  CallbackModule :: module(),
                  InitArg :: any()) -> ok.
add_handler(Manager, CallbackModule, InitArg) ->
    ok = gen_event:add_sup_handler(
           ?registry(Manager), ?MODULE, {CallbackModule, InitArg}).

%% @doc Notify the event manager that an RPC Request has been received.
-spec rpc_request(EventManager :: pid(), Request :: term()) -> ok.
rpc_request(EventManager, Request) ->
    gen_event:notify(EventManager, {rpc_request, Request}).

%% @doc Notify the event manager that an RPC Response has been received.
-spec rpc_response(EventManager :: pid(), Response :: term()) -> ok.
rpc_response(EventManager, Response) ->
    gen_event:notify(EventManager, {rpc_response, Response}).

init({CallbackModule, InitArg} = Handler) ->
    case CallbackModule:init(InitArg) of
        {ok, State} ->
            {ok, #state{handler_state = State,
                        handler = Handler}};
        {error, _} = Error ->
            Error
    end.

handle_event(Event, State) ->
    io:format("got event ~p while state is ~p~n", [Event, State]),
    {ok, State}.

handle_call(Call, State) ->
    io:format("got call ~p while state is ~p~n", [Call, State]),
    {ok, error, State}.
