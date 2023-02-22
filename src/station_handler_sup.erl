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

-export([start_link/1, start_handler/2]).

-export([init/1]).

start_link(HandlerModule) ->
    supervisor:start_link(?MODULE, [HandlerModule]).

%% @doc Start a new station handler.
-spec start_handler(Supervisor :: pid(), StationName :: binary()) ->
          supervisor:startchild_ret().
start_handler(Supervisor, StationName) ->
    supervisor:start_child(Supervisor, [StationName]).

init([HandlerModule]) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 3,
                 period => 1},
    ChildSpecs = [#{id => station_handler,
                    start => {station_handler, start_link, [HandlerModule]},
                    restart => temporary,
                    type => worker,
                    modules => [station_handler]}],
    {ok, {SupFlags, ChildSpecs}}.
