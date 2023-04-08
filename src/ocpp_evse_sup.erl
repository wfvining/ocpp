%%% @doc Supervisor for EVSE processes associated with a single station.
%%% @end
%%%
%%% Copyright (c) 2023 Will Vining <wfv@vining.dev>

-module(ocpp_evse_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(EVSE_SPEC(EVSEId),
        #{id => {evse, EVSEId},
          start => {ocpp_evse, start_link, [EVSEId]},
          restart => permanent,
          type => worker,
          modules => [ocpp_evse]}).

start_link(NumEVSE) ->
    supervisor:start_link(?MODULE, NumEVSE).

init(NumEVSE) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},
    Children = [?EVSE_SPEC(EVSEId) || EVSEId <- lists:seq(1, NumEVSE)],
    {ok, {SupFlags, Children}}.
