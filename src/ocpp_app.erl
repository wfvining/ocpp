%%% @doc ocpp application
%%% @end
%%%
%%% Copyright (c) 2023 Will Vining <wfv@vining.dev>

-module(ocpp_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Options =
        [{path, application:get_env(ocpp, wspath, "/ocpp")},
         {port, application:get_env(ocpp, wsport, 3443)}],
    ocpp_sup:start_link(Options).

stop(_State) ->
    ok.

%% internal functions
