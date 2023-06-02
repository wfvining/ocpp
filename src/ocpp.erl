%%%-------------------------------------------------------------------
%% @doc ocpp public API
%% @end
%%%-------------------------------------------------------------------

-module(ocpp).

-export([add_station/3]).

%% @doc Add a station to the CSMS. If succesful the Pid of the Station
%% Manager process is returned. No action will be taken if a station
%% with the same `StationId' has already been added. If the `Handler'
%% returns an error from or fails in its `init/1' callback the value
%% `{error, {init, Reason}}' is returned where `Reason' is the error
%% reason from `HandlerModule:init/1'.
%%
%% @param StationId the name of the station. Must be unique.
%% @param NumEVSE the number of EVSE connected to this station.
%% @param Handler a tuple specifying the callback module that will
%%        handler OCPP requests for the CSMS and the argument passed
%%        to its `init/1` callback.
%%
%% @end
-spec add_station(StationId :: binary(),
                  NumEVSE :: pos_integer(),
                  Handler :: {module(), any()}) ->
          {ok, StationManager :: pid()} | {error, Reason}
              when Reason :: already_exists
                           | {init, any()}.
add_station(StationId, NumEVSE, Handler) ->
    ocpp_manager:add_station(StationId, NumEVSE, Handler).

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
