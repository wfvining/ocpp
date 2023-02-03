%%%-------------------------------------------------------------------
%% @doc ocpp public API
%% @end
%%%-------------------------------------------------------------------

-module(ocpp).

-behaviour(application).

%% application callbacks
-export([start/2, stop/1]).
%% API
-export([start_station_handler/2]).

%% API functions
-spec start_station_handler(StationName :: binary(), Options :: map()) ->
          {ok, Handler :: pid()} |
          {error, Reason :: any()}.
start_station_handler(StationName, Options) ->
    station_handler_sup:start_handler(StationName, Options).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile(
                 [{'_', [{"/ocpp/:csname", ocpp_websocket_handler, []}]}]),
    {ok, _} = cowboy:start_clear(
                ocppj,
                [{port, 3443}],
                #{env => #{dispatch => Dispatch}}),
    ocpp_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
