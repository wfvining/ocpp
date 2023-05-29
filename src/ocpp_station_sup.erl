%%% @doc Supervisor for `ocpp_station' processes.
%%% @end
%%%
%%% Copyright 2023 (c) Will Vining <wfv@vining.dev>

-module(ocpp_station_sup).

-behaviour(supervisor).

-export([start_link/3, start_evse_sup/1, stop/1]).
-export([init/1]).

start_link(StationId, NumEVSE, CSMSEventManager) ->
    supervisor:start_link(?MODULE, {StationId, NumEVSE, CSMSEventManager}).

init({StationId, NumEVSE, CSMSEventManager}) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 3,
                 period => 3600},
    ChildSpecs =
        [#{id => station,
           start => {ocpp_station, start_link, [StationId, NumEVSE, CSMSEventManager]},
           restart => permanent,
           type => worker,
           shutdown => 10000,
           modules => [ocpp_station, ocpp_station_registry]}],
    {ok, {SupFlags, ChildSpecs}}.

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
