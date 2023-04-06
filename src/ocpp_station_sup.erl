%%% @doc Supervisor for `ocpp_station' processes.
%%% @end
%%%
%%% Copyright 2023 (c) Will Vining <wfv@vining.dev>

-module(ocpp_station_sup).

-behaviour(supervisor).

-export([start_link/0, start_station/1]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec start_station(StationName :: binary()) -> supervisor:startchild_ret().
start_station(StationName) ->
    supervisor:start_child(?SERVER, [StationName, self()]).

init([]) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 10,
                 period => 3},
    ChildSpec = #{id => station,
                  start => {ocpp_station, start_link, []},
                  %% don't want these to ever restart.
                  restart => temporary,
                  type => worker,
                  modules => [ocpp_station]},
    {ok, {SupFlags, [ChildSpec]}}.
