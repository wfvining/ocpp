%%% @doc Supervisor for station management related processes.
%%% @end
%%%
%%% Copyright 2023 (c) Will Vining <wfv@vining.dev>

-module(ocpp_csms_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => rest_for_one,
                 intensity => 2,
                 period => 3600},
    ChildSpecs = [#{id => ocpp_manager,
                    start => {ocpp_manager, start_link, []},
                    type => worker,
                    shutdown => 5000,
                    modules => [ocpp_manager]},
                  #{id => ocpp_staion_manager_supersup,
                    start => {ocpp_station_manager_supersup, start_link, []},
                    type => supervisor,
                    modules => [ocpp_station_manager_supersup]}],
    {ok, {SupFlags, ChildSpecs}}.
