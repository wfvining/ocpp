%%% Copyright (c) 2023 Will Vining <wfv@vining.dev>

-module(ocpp_evse_test).

-include_lib("eunit/include/eunit.hrl").

invalid_connector_test_() ->
    EVSE = ocpp_evse:new(2),
    {inparallel,
     [?_assertError(invalid_connectorid,
                    ocpp_evse:set_status(EVSE, Id, 'Available'))
      || Id <- [-1, 0, 3]]}.

set_status_test_() ->
    EVSE = ocpp_evse:new(3),
    {inparallel,
     [[?_assertEqual(
          if C =:= Connector -> Status; C =/= Connector -> 'Unavailable' end,
          ocpp_evse:status(ocpp_evse:set_status(EVSE, Connector, Status), C))
       || C <- [1, 2, 3]]
      || {Connector, Status} <- [{1, 'Available'},
                                 {2, 'Faulted'},
                                 {3, 'Reserved'}]]}.
