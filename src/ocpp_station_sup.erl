%%% @doc Supervisor for `ocpp_station' processes.
%%% @end
%%%
%%% Copyright 2023 (c) Will Vining <wfv@vining.dev>

-module(ocpp_station_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

-define(SPEC(Station),
        #{id => Station,
          start => {ocpp_station, start_link, [Station]},
          %% Should not restart on normal termination because stations
          %% may be removed.
          restart => transient,
          type => worker,
          modules => [ocpp_station]}).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 3},
    {ok, {SupFlags, station_specs()}}.

station_specs() ->
    [?SPEC(Station) || Station <- ocpp_station_db:all_stations()].
