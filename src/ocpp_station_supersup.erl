%%% @doc Top level supervisor for stations.
%%% @end
%%%
%%% Copyright 2023 (c) Will Vining <wfv@vining.dev>

-module(ocpp_station_supersup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

-define(SPEC(StationId, NumEVSE),
        #{id => StationId,
          start => {ocpp_station_sup, start_link, [StationId, NumEVSE]},
          %% Should not restart on normal termination because stations
          %% may be removed.
          restart => transient,
          type => supervisor,
          modules => [ocpp_station_sup]}).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 1,
                 period => 3},
    StationRegistry = #{id => station_registry,
                        start => {ocpp_station_registry, start_link, []},
                        type => worker,
                        restart => permanent},
    StationSpecs = station_sup_specs(),
    {ok, {SupFlags, [StationRegistry|StationSpecs]}}.

station_sup_specs() ->
    [?SPEC(StationId, NumEVSE)
     || #{id := StationId, num_evse := NumEVSE}
            <- ocpp_station_db:all_stations()].
