%%%-------------------------------------------------------------------
%% @doc ocpp public API
%% @end
%%%-------------------------------------------------------------------

-module(ocpp).

-export([start_ocpp_server/1]).

-spec start_ocpp_server(Options :: proplists:proplist()) -> {ok, pid()}.
start_ocpp_server(Options) ->
    BasePath = proplists:get_value(path, Options, "/ocpp"),
    Port = proplists:get_value(port, Options, 3443),
    Dispatch = cowboy_router:compile(
                 [{'_', [{BasePath ++ "/:csname",
                          ocpp_websocket_handler,
                          []}]}]),
    {ok, _} = cowboy:start_clear(
                ocppj,
                [{port, Port}],
                #{env => #{dispatch => Dispatch}}).
