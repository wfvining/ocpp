-module(ocpp_station_manager_supersup).

-behaviour(supervisor).

-export([start_link/0, add_station/3]).
-export([init/1]).

-define(SERVER, ?MODULE).

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% @doc Add `StationName' to the set of stations managed by this
%% application.

%% @param StationName The name of the station.
%%
%% @param NumEVSE The number of EVSE in the station.
%%
%% @param Handler The callback module implementing the
%% `ocpp_handler' behavior and the value that should be passed
%% to its `init/1' callback.
%% @end
-spec add_station(StationName :: binary(),
                  NumEVSE :: pos_integer(),
                  Handler :: {Mod :: module(), InitArg :: any()}) ->
          supervisor:startchild_ret().
add_station(StationName, NumEVSE, Handler) ->
    supervisor:start_child(
      ?SERVER, [StationName, NumEVSE, Handler]).

init([]) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 2,
                 period => 3600},
    ChildSpec = [#{id => ocpp_station_manager_sup,
                   start => {ocpp_station_manager_sup, start_link, []},
                   type => supervisor,
                   restart => transient,
                   modules => [ocpp_station_manager_sup]}],
    {ok, {SupFlags, ChildSpec}}.
