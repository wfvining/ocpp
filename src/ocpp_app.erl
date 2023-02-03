%%%-------------------------------------------------------------------
%% @doc ocpp public API
%% @end
%%%-------------------------------------------------------------------

-module(ocpp_app).

-behaviour(application).

-export([start/2, stop/1]).

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
