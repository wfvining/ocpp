%%%-------------------------------------------------------------------
%% @doc Supervisor for charging station connection handler processes.
%%
%% This is a `simple_one_for_one' supervisor that starts a process to
%% parse and dispatch messages received from a charging station.
%% @end
%%
%% Copyright (c) 2023 Will Vining <wfv@vining.dev>
%%%-------------------------------------------------------------------

-module(station_handler_sup).

-behaviour(supervisor).

-export([start_link/0, start_handler/2]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% @doc Start a new station handler.
-spec start_handler(StationName :: binary(), Options :: proplists:proplist()) ->
          {ok, pid()} |
          {ok, undefined} |
          {error, any()}.
start_handler(StationName, Options) ->
    supervisor:start_child(?SERVER, [StationName, Options]).

init([]) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 3,
                 period => 1},
    ChildSpecs = [#{id => station_handler,
                    start => {station_handler, start_link, []},
                    restart => transient,
                    type => worker,
                    modules => [station_handler]}],
    {ok, {SupFlags, ChildSpecs}}.
