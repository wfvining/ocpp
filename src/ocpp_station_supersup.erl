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
    {ok, {SupFlags, [#{id => station_sup,
                       start => {ocpp_station_sup, start_link, []},
                       type => supervisor,
                       restart => transient}]}}.

-spec start_station(StationId :: binary(),
                    EVSE :: [ocpp_evse:evse()],
                    CSMSHandler :: {Module :: module(), InitArg :: any()}) ->
          {ok, pid()} |
          {error, {already_started, pid()}}.
start_station(StationId, NumEVSE, CSMSHandler) ->
    case ocpp_station_manager:whereis(StationId) of
        undefined ->
            do_start_station(StationId, NumEVSE, CSMSHandler);
        Pid when is_pid(Pid) ->
            {error, {already_started, Pid}}
    end.

do_start_station(StationId, NumEVSE, CSMSHandler) ->
    case supervisor:start_child(
           ?SERVER, [StationId, NumEVSE, CSMSHandler])
    of
        {ok, _} = Ok -> Ok;
        {error, {shutdown, {failed_to_start_child, _, Err}}} ->
            Err
    end.
