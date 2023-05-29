%%% @doc Top level supervisor for stations.
%%% @end
%%%
%%% Copyright 2023 (c) Will Vining <wfv@vining.dev>

-module(ocpp_station_supersup).

-behaviour(supervisor).

-export([start_link/0, start_station/3]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 2,
                 period => 3600},
    ocpp_station_registry:new(),
    {ok, {SupFlags, [#{id => station_sup,
                       start => {ocpp_station_sup, start_link, []},
                       type => supervisor,
                       restart => transient}]}}.

-spec start_station(StationId :: binary(),
                    NumEVSE :: pos_integer(),
                    CSMSEventManager :: pid()) ->
          {ok, pid()} |
          {error, {already_started, pid()}}.
start_station(StationId, NumEVSE, CSMSEventManager) ->
    case ocpp_station_registry:lookup_station(StationId) of
        {ok, Pid} ->
            {error, {already_started, Pid}};
        {error, unregistered} ->
            supervisor:start_child(?SERVER, [StationId, NumEVSE, CSMSEventManager])
    end.
