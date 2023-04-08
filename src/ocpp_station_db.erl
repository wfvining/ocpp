%%% @doc interface to the table of charging stations.
%%%
%%% Copyright (c) 2023 Will Vining <wfv@vining.dev>
-module(ocpp_station_db).

-include_lib("stdlib/include/qlc.hrl").

-export([install/1, add_station/2, get_station/1, id/1, all_stations/0]).

-export_type([station/0]).

-record(charging_station, {id :: binary(), num_evse :: pos_integer()}).

-opaque station() :: #charging_station{}.

-define(OCPP_MAX_ID_LEN, 48).

install(Nodes) ->
    mnesia:create_table(charging_station,
                        [{attributes, record_info(fields, charging_station)},
                         {disc_copies, Nodes}]).

-spec add_station(Id :: binary(), NumEvse :: pos_integer()) -> ok.
add_station(Id, NumEvse) when NumEvse > 0 ->
    Len = string:length(Id),
    if Len > ?OCPP_MAX_ID_LEN ->
            error(id_too_long);
       true ->
            mnesia:transaction(
              fun() ->
                      mnesia:write(#charging_station{
                                      id = Id,
                                      num_evse = NumEvse})
              end)
    end.

get_station(Id) ->
    F = fun() -> mnesia:read({charging_station, Id}) end,
    case mnesia:transaction(F) of
        {atomic, [Station]} -> {ok, Station};
        {atomic, []} -> notfound
    end.

id(#charging_station{id=StationId}) ->
    StationId.

%% @doc Return a list of all stations.
-spec all_stations() -> [#{id := binary(), num_evse := pos_integer()}].
all_stations() ->
    F = fun() -> qlc:e(
                   qlc:q([#{id => Station#charging_station.id,
                            num_evse => Station#charging_station.num_evse}
                          || Station <- mnesia:table(charging_station)]))
        end,
    case mnesia:transaction(F) of
        {atomic, Stations} -> Stations
    end.
