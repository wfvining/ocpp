%%% @doc Data structure representing an EVSE.
%%% @end
%%%
%%% Copyright (c) 2023 Will Vining <wfv@vining.dev>

-module(ocpp_evse).

-export([new/1, new_connector/0, new_connector/1, set_status/3,
         set_evse_status/2, status/2, add_connector/3]).

-export_type([evse/0, connector/0, status/0]).

-record(connector, {status = 'Available' :: status()}).
-record(evse, {connectors = [] :: [connector()], status = 'Available' :: status()}).

-opaque connector() :: #connector{}.
-opaque evse() :: #evse{}.

-type status() :: 'Available'
                | 'Occupied'
                | 'Reserved'
                | 'Unavailable'
                | 'Faulted'.

%% @doc Create a new EVSE. `Connectors' can be a list of `connector/0'
%% structures or a non-zero integer. If it is an integer the evse will
%% be initialized with that number of connectors, all in the
%% 'Unavailable' state.
-spec new(Connectors :: pos_integer() | [connector()]) -> evse().
new(Connectors) when is_integer(Connectors) ->
    #evse{connectors = lists:duplicate(Connectors, #connector{})};
new(Connectors) ->
    #evse{connectors = Connectors}.

-spec add_connector(EVSE :: evse(), ConnectorId :: pos_integer(), Status :: status()) -> evse().
add_connector(#evse{connectors = Connectors} = EVSE, ConnectorId, Status) ->
    Add = length(Connectors) - ConnectorId,
    NewConn = lists:duplicate(Add - 1, new_connector()) ++ [new_connector(Status)],
    EVSE#evse{connectors = Connectors ++ NewConn}.

%% @doc Return an ``'Unavailable' '' connector.
-spec new_connector() -> connector().
new_connector() ->
    #connector{}.

%% @doc Return a connector with status set to `Status'
-spec new_connector(Status :: status()) -> connector().
new_connector(Status) ->
    #connector{status = Status}.

-spec status(EVSE :: evse(), ConnectorId :: pos_integer()) -> status().
status(#evse{connectors = Connectors}, ConnectorId) ->
    Conn = lists:nth(ConnectorId, Connectors),
    Conn#connector.status.

-spec set_status(EVSE :: evse(),
                 ConnectorId :: pos_integer(),
                 Status :: status()) -> evse().
set_status(#evse{connectors = Connectors} = EVSE, ConnectorId, Status)
  when ConnectorId > length(Connectors);
       ConnectorId < 1 ->
    error(badconnector, [EVSE, ConnectorId, Status]);
set_status(#evse{connectors = Connectors} = EVSE, ConnectorId, Status) ->
    EVSE#evse{
      connectors =
          do_set_status(Connectors, ConnectorId - 1, Status)}.

set_evse_status(EVSE, Status) ->
    EVSE#evse{status = Status}.

do_set_status([Connector|Rest], 0, Status) ->
    [Connector#connector{status = Status} | Rest];
do_set_status([Conn|Rest], Index, Status) ->
    [Conn | do_set_status(Rest, Index - 1, Status)].
