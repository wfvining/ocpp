%%%-------------------------------------------------------------------
%% @doc ocpp public API
%% @end
%%%-------------------------------------------------------------------

-module(ocpp).

-export([start_ocpp_server/2]).

-spec start_ocpp_server(HandlerModule :: module(),
                        Options :: proplists:proplist()) ->
          {ok, pid()}.
start_ocpp_server(HandlerModule, Options) ->
    BasePath = proplists:get_value(path, Options, "/ocpp"),
    Port = proplists:get_value(port, Options, 3443),
    {ok, Server} = ocpp_server_supersup:start_ocpp_server([HandlerModule]),
    Dispatch = cowboy_router:compile(
                 [{'_', [{BasePath ++ "/:csname",
                          ocpp_websocket_handler,
                          [Server]}]}]),
    {ok, _} = cowboy:start_clear(
                ocppj,
                [{port, Port}],
                #{env => #{dispatch => Dispatch}}),
    {ok, Server}.
