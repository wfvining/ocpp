%%% @doc interface to the table of charging stations.
%%%
%%% Copyright (c) 2023 Will Vining <wfv@vining.dev>
-module(ocpp_station_db).

-export([install/1, add_station/2, get_station/1, id/1]).

-record(charging_station, {id :: binary(), num_evse :: pos_integer()}).

-define(OCPP_MAX_ID_LEN, 48).

install(Nodes) ->
    mnesia:create_table(charging_station,
                        [{attributes, record_info(fields, charging_station)},
                         {disc_copies, Nodes}]).

-spec add_station(Id :: binary(), NumEvse :: pos_integer()) -> ok.
add_station(Id, NumEvse) ->
    Len = string:length(Id),
    if Len > ?OCPP_MAX_ID_LEN ->
            mnesia:abort(id_too_long);
       true ->
            mnesia:write(#charging_station{id = Id, num_evse = NumEvse})
    end.

get_station(Id) ->
    F = fun() -> mnesia:read({charging_station, Id}) end,
    case mnesia:transaction(F) of
        {atomic, [Station]} -> {ok, Station};
        {atomic, []} -> notfound
    end.

id(#charging_station{id=StationId}) ->
    StationId.
