%%% @doc interface to the table of charging stations.
%%%
%%% Copyright (c) 2023 Will Vining <wfv@vining.dev>
-module(ocpp_station_db).

-export([install/1, add_station/2]).

-record(charging_station, {id :: binary(), num_evse :: pos_integer()}).

install(Nodes) ->
    mnesia:create_table(charging_station,
                        [{attributes, record_info(fields, charging_station)},
                         {disc_copies, Nodes}]).

-spec add_station(Id :: binary(), NumEvse :: pos_integer()) -> ok.
add_station(Id, NumEvse) ->
    mnesia:write(#charging_station{id = Id, num_evse = NumEvse}).
