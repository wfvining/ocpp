%%% @doc Supervisor for `ocpp_station' processes.
%%% @end
%%%
%%% Copyright 2023 (c) Will Vining <wfv@vining.dev>

-module(ocpp_station_sup).

-behaviour(supervisor).

-export([start_link/2]).
-export([init/1]).

start_link(StationId, NumEVSE) ->
    supervisor:start_link(?MODULE, {StationId, NumEVSE}).

init({StationId, NumEVSE}) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 3},
    ChildSpecs = [#{id => evse_sup,
                    start => {ocpp_evse_sup, start_link, [NumEVSE]},
                    restart => permanent,
                    type => supervisor,
                    modules => [ocpp_evse_sup]},
                  #{id => station,
                    start => {ocpp_station, start_link, [StationId]},
                    restart => permanent,
                    type => supervisor,
                    modules => [ocpp_station]}],
    {ok, {SupFlags, ChildSpecs}}.
