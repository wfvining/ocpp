# Open Charge Point Porotcol

An application implementating the Open Charge Point Protocol, version
2.0.1. This application is designed to be used as the communication
layer for a charging station management system (CSMS). The application
is designed to minimize, as much as possible, the implementation
burden for a CSMS by handling requests automatically wherever possible
(e.g. `StatusNotification` of `Heartbeat` messages). Furthermore, it
endeavors to enforce compliant behavior with respect to the OCPP
standard; a CSMS implemented using this application should not need to
be concerned with message ordering or low-level semantics, only with
the business logic of managing charging stations.

## Architecture

Charging stations are modeled in the application as a collection of
processes, the primary process being the `ocpp_station` state
machine. This process indirectly manages all communication between the
CSMS and the charging station. This module exposes an API that is used
by the process that directly manages the websocket connection between
the CSMS and the charging station. All communication between the
`ocpp_station` process and the websocket process takes place over
Erlang messaging and consists of Erlang representations of OCPP
messages.

Whenever the station sends a message that cannot be handled
automatically the message is forwarded to an event manager which
notifies the CSMS that there is a message which requires its
attention. Once the CSMS has processed the message it notifies the
`ocpp_station` process what the response to the station should be.

## The `ocpp_handler` behavior

The `ocpp_handler` behavior is the integration point between a CSMS
application and the `ocpp` application. It is used by CSMS
implementers to provide callbacks that are invoked for each OCPP
message type. The values returned from the callbacks are used by the
`ocpp_station` to construct the response message.

## Future work

- Websocket server
- Client API
