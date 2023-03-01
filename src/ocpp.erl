%%%-------------------------------------------------------------------
%% @doc ocpp public API
%% @end
%%%-------------------------------------------------------------------

-module(ocpp).

-export([start_ocpp_server/1, install/1, add_charging_station/2]).

-spec install(Nodes :: [node()]) -> ok.
install(Nodes) ->
    ok = mnesia:create_schema(Nodes),
    application:start(mnesia),
    ocpp_station_db:install(Nodes),
    ocpp_authentication:install(Nodes),
    application:stop(mnesia).

-spec add_charging_station(Id :: binary(), NumEvse :: pos_integer()) ->
          {ok, Password :: binary()} | {error, Reason :: any()}.
add_charging_station(Id, NumEvse) ->
    F = fun() ->
                ocpp_station_db:add_station(Id, NumEvse),
                ocpp_authentication:set_password(Id)
        end,
    case mnesia:transaction(F) of
        {aborted, exists} ->
            {error, exists};
        {atomic, Password} ->
            {ok, Password}
    end.

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
