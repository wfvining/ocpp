-record(evse, {id = 1 :: pos_integer(),
               connectors = []}).

-type evse() :: #evse{}

-record(charging_station,
        {evse = [] :: [evse()]}).
