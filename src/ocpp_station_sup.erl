%%% @doc Supervisor for `ocpp_station' processes.
%%% @end
%%%
%%% Copyright 2023 (c) Will Vining <wfv@vining.dev>

-module(ocpp_station_sup).

-behaviour(supervisor).

-export([start_link/3, stop/1]).
-export([init/1]).

-spec start_link(StationId :: binary(),
                 EVSE :: [ocpp_evse:evse()],
                 CSMSHandler :: {module(), any()}) ->
          supervisor:startlink_ret().
start_link(StationId, EVSE, CSMSHandler) ->
    supervisor:start_link(?MODULE, {StationId, EVSE, CSMSHandler}).

init({StationId, EVSE, CSMSHandler}) ->
    SupFlags = #{strategy => rest_for_one,
                 intensity => 4,
                 period => 3600},
    ChildSpecs =
        [#{id => station_event_manager,
           start => {ocpp_handler, start_link, [StationId]},
           type => worker,
           %% giving a long time since this will involve user code.
           shutdown => 10000,
           restart => transient,
           modules => dynamic},
          #{id => station_manager,
           start => {ocpp_station_manager, start_link,
                     [StationId, CSMSHandler]},
           type => worker,
           restart => permanent,
           shutdown => 1000,
           modules => [ocpp_station_manager]},
         #{id => station,
           start => {ocpp_station, start_link,
                     [StationId, EVSE]},
           restart => permanent,
           type => worker,
           shutdown => 10000,
           modules => [ocpp_station]}],
    {ok, {SupFlags, ChildSpecs}}.

-spec stop(StationSupervisor :: pid()) -> true.
stop(StationSupervisor) ->
    exit(StationSupervisor, shutdown).
