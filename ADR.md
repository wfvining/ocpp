# Architecture Decision Records

Each record must include a description of the decision, the
context in which the decision was made, and a discussion of the
consequences of the decision.

## The EVSE module must not depend on the Station module

### Summary

Enforce strict layering between the Station and the EVSE modules, with
the EVSE module subordinate to the Station.

### Context

The original plan for the EVSE module was that after starting, it
should register itself with its station. This would give the station
access to the EVSE that belong to it, enabling it to send commands to
individual EVSE. While developing tests for the Station module
(`ocpp_station`) and working on setup functions to start the station
process for the tests the strong coupling between the EVSE and the
Station module became clear. It was impossible to properly test the
station without an implementation of the EVSE module containing at
least enough functionality that it could register itself with the
station. This is too much coupling, and violates the logical view
that EVSE are subordinate to the station.

### Consequences

By requiring strict layering between the station and EVSE module,
allowing the station to call on EVSE but not vice-versa, we can reduce
coupling with the station module in the EVSE module. The station
module can now be completely replaced without impacting the EVSE
module. Additionally, the station no longer depends on the behavior of
the EVSE since it doesn't rely on registration messages. This comes at
the cost of slightly increased coupling between the station, its
supervisor, and the EVSE supervisor (required for the station to
dynamically start the EVSE supervisor and EVSE processes). This
decision also makes is much simpler to add EVSE to a station after the
CSMS has started, increasing the flexibility of the running system.
