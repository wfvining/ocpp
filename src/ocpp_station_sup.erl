%%% @doc Supervisor for `ocpp_station' processes.
%%% @end
%%%
%%% Copyright 2023 (c) Will Vining <wfv@vining.dev>

-module(ocpp_station_sup).

-behaviour(supervisor).

-export([start_link/3, start_event_manager/1, start_evse_sup/1,
         start_station/4, stop/1]).
-export([init/1]).

start_link(StationId, NumEVSE, CSMSHandler) ->
    supervisor:start_link(?MODULE, {StationId, NumEVSE, CSMSHandler}).

init({StationId, NumEVSE, CSMSHandler}) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 3,
                 period => 3600},
    ChildSpecs =
        [#{id => station_manager,
           start => {ocpp_station_manager, start_link,
                     [self(), StationId, NumEVSE, CSMSHandler]},
           type => worker,
           restart => permanent,
           shutdown => 1000,
           modules => [ocpp_station_manager]}],
    {ok, {SupFlags, ChildSpecs}}.

-spec start_event_manager(Supervisor :: pid()) -> supervisor:startchild_ret().
start_event_manager(Supervisor) ->
    supervisor:start_child(
      Supervisor,
      #{id => station_event_manager,
        start => {ocpp_handler, start_link, []},
        type => worker,
        %% giving a long time since this will involve user code.
        shutdown => 10000,
        restart => transient,
        modules => dynamic}).


-spec start_station(Supervisor :: pid(),
                    StationId :: binary(),
                    NumEVSE :: pos_integer(),
                    CSMSEventManager :: pid()) -> supervisor:startchild_ret().
start_station(Supervisor, StationId, NumEVSE, CSMSEventManager) ->
    supervisor:start_child(
      Supervisor,
      #{id => station,
        start => {ocpp_station, start_link,
                  [StationId, NumEVSE, CSMSEventManager]},
        restart => permanent,
        type => worker,
        shutdown => 10000,
        modules => [ocpp_station, ocpp_station_registry]}).

-spec start_evse_sup(StationSupervisor :: pid()) -> {ok, pid()}.
start_evse_sup(StationSupervisor) ->
    supervisor:start_child(
      StationSupervisor,
      #{id => evse_sup,
        start => {ocpp_evse_sup, start_link, []},
        restart => temporary,
        type => supervisor,
        modules => [ocpp_evse_sup]}).

-spec stop(StationSupervisor :: pid()) -> true.
stop(StationSupervisor) ->
    exit(StationSupervisor, shutdown).
