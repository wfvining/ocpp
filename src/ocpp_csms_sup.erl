%%% @doc Supervisor for station management related processes.
%%% @end
%%%
%%% Copyright 2023 (c) Will Vining <wfv@vining.dev>

-module(ocpp_csms_sup).

-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link(Options) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Options).

init(Options) ->
    SupFlags = #{strategy => rest_for_one,
                 intensity => 2,
                 period => 3600},
    ChildSpecs = [#{id => ocpp_manager,
                    start => {ocpp_manager, start_link, [Options]},
                    type => worker,
                    shutdown => 5000,
                    modules => [ocpp_manager]},
                  #{id => ocpp_csms,
                    start => {ocpp_csms, start_link, []},
                    type => worker,
                    shutdown => 5000,
                    modules => [ocpp_csms]}],
    {ok, {SupFlags, ChildSpecs}}.
