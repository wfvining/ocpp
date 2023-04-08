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
          modules => [station_sup_specs]}).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 1,
                 period => 3},
    StationSpecs = station_sup_specs(),
    {ok, {SupFlags, StationSpecs}}.

station_sup_specs() ->
    [?SPEC(StationId, NumEVSE)
     || #{id := StationId, num_evse := NumEVSE}
            <- ocpp_station_db:all_stations()].
