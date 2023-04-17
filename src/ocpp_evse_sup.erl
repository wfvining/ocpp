%%% @doc Supervisor for EVSE processes associated with a single station.
%%% @end
%%%
%%% Copyright (c) 2023 Will Vining <wfv@vining.dev>

-module(ocpp_evse_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_evse/2]).

%% Supervisor callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link(?MODULE, []).

init([]) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 1,
                 period => 3600},
    {ok, {SupFlags, [#{id => evse,
                       start => {ocpp_evse, start_link, []},
                       restart => transient,
                       type => worker,
                       modules => [ocpp_evse]}]}}.

-spec start_evse(EVSESupervisor :: pid(),
                 EVSEId :: pos_integer()) -> {ok, pid()}.
start_evse(EVSESupervisor, EVSEId) ->
    supervisor:start_child(EVSESupervisor, [EVSEId]).
