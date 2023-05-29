-module(ocpp_station_manager).

-behaviour(gen_server).

-export([start_link/4]).
-export([init/1, handle_continue/2,
         handle_call/3, handle_cast/2,
         handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).

-record(state, {handler :: {module(), any()},
                event_manager = undefined :: undefined | pid(),
                supervisor :: pid()}).

-spec start_link(Supervisor :: pid(),
                 StationName :: binary(),
                 NumEVSE :: pos_integer(),
                 CSMSHandler :: {Module :: module(), InitArg :: any()}) ->
          gen_server:start_ret().
start_link(Supervisor, StationName, NumEVSE, CSMSHandler) ->
    gen_server:start_link(
      ?MODULE,
      {Supervisor, StationName, NumEVSE, CSMSHandler},
      []).

init({Supervisor, StationName, NumEVSE, HandlerCallBackModule}) ->
    process_flag(trap_exit, true),
    {ok,
     #state{handler = HandlerCallBackModule,
            supervisor = Supervisor},
     {continue, {initialize_station, {StationName, NumEVSE}}}}.

handle_continue({initialize_station, {StationName, NumEVSE}},
                #state{handler = {Module, InitArg}} = State) ->
    %% 1. Start the event manager
    {ok, EventManager} =
        ocpp_station_manager_sup:start_event_manager(State#state.supervisor),
    %% 2. Add the handler
    ok = ocpp_handler:add_handler(
           EventManager,
           Module,
           InitArg),
    %% 3. Start the station fsm
    case ocpp_station_supersup:start_station(
           StationName,
           NumEVSE,
           EventManager)
    of
        {ok, _Pid} ->
            {noreply, State#state{event_manager = EventManager}};
        {error, Reason} ->
            {stop, Reason, State}
    end.

handle_call(Call, _From, State) ->
    logger:warning("Unexpected call ~p", [Call]),
    {noreply, State}.

handle_cast(Cast, State) ->
    logger:warning("Unexpected cast ~p", [Cast]),
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
