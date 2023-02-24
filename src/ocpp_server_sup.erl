%%%-------------------------------------------------------------------
%% @doc Supervisor for an OCPP server.
%% @end
%%
%% Copyright (c) 2023 Will Vining <wfv@vining.dev>
%%%-------------------------------------------------------------------

-module(ocpp_server_sup).

-behaviour(supervisor).

-export([start_link/1, start_handler/2]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link(HandlerModule) ->
    supervisor:start_link(?MODULE, [HandlerModule]).

%% @doc Start a new station handler.
-spec start_handler(StationName :: binary(),
                    Options :: proplists:proplist()) ->
          {ok, pid()} |
          {ok, undefined} |
          {error, any()}.
start_handler(StationName, Options) ->
    supervisor:start_child(?SERVER, [StationName, Options]).

init([HandlerModule]) ->
    %% TODO start a server to manage the handlers and a handler supervisor
    SupFlags = #{strategy => one_for_all,
                 intensity => 3,
                 period => 1},
    ChildSpecs = [#{id => ocpp_server,
                    start => {ocpp_server, start_link, [HandlerModule]},
                    restart => permanent,
                    type => worker,
                    modules => [ocpp_server]}],
    {ok, {SupFlags, ChildSpecs}}.
