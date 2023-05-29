-module(ocpp_station_manager_sup).

-behaviour(supervisor).

-export([start_link/3, start_event_manager/1]).
-export([init/1]).

start_link(StationName, NumEVSE, Handler) ->
    supervisor:start_link(
      ?MODULE,
      {StationName, NumEVSE, Handler}).

start_event_manager(Supervisor) ->
    supervisor:start_child(
      Supervisor,
      #{id => ocpp_station_event_manager,
        start => {ocpp_handler, start_link, []},
        type => worker,
        %% giving a long time since this will involve user code.
        shutdown => 10000,
        restart => transient,
        modules => dynamic}).

init({StationName, NumEVSE, HandlerCallBackModule}) ->
    SupFlags = #{startegy => simple_one_for_one,
                 intensity => 1,
                 period => 3600},
    ChildSpecs = [#{id => ocpp_station_manager,
                    start => {ocpp_station_manager, start_link,
                              [self(), StationName, NumEVSE, HandlerCallBackModule]},
                    type => worker,
                    restart => transient,
                    modules => [ocpp_station_manager]}],
    {ok, {SupFlags, ChildSpecs}}.
