%%%-------------------------------------------------------------------
%% @doc Top level supervisor for OCPP server processes.
%% @end
%%
%% Copyright (c) 2023 Will Vining <wfv@vining.dev>
%%%-------------------------------------------------------------------

-module(ocpp_server_supersup).

-behaviour(supervisor).

-export([start_link/0, start_ocpp_server/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec start_ocpp_server(HandlerModule :: module()) -> supervisor:startchild_ret().
start_ocpp_server(HandlerModule) ->
    supervisor:start_child(?SERVER, [HandlerModule]).

init([]) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 3,
                 period => 1},
    ChildSpecs = [#{id => ocpp_server_sup,
                    start => {ocpp_server_sup, start_link, []},
                    restart => permanent,
                    type => supervisor,
                    modules => [ocpp_server_sup]}],
    {ok, {SupFlags, ChildSpecs}}.
