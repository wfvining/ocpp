%%% @doc ocpp application
%%% @end
%%%
%%% Copyright (c) 2023 Will Vining <wfv@vining.dev>

-module(ocpp_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ocpp_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
