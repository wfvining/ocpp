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

## Usage

The `ocpp` application doesn't do anything on its own. It is designed
to be used as the protocol layer in release consisting of a websocket
server and a CSMS application. I'll try to provide a description of
how the `ocpp` application should be used in this context. The example
application has three layers. First there is a communication layer
that handles the interaction (via the websocket-based OCPP-j standard)
between the charging stations and the CSMS. At the other end of the
stack there is a CSMS application that provides a management interface
for use by a charging station/network operator. Between these two
layers sits the `ocpp` application. It's role is to act as a middle
man between the two applications, enforcing the OCPP standard and
managing a model of the state of the stations in the charging network.

### Websocket layer

The websocket layer could be implemented using `cowboy` in a
`websocket_handler`. The websocket handler needs to declare to the
`ocpp` application that it is connected to the station by calling
`ocpp_station:connect/1`. Once connected, the station process will
forward any outgoing messages to the handler so they can be sent to
the station (more on this later). Whenever an OCPP-j RPC message is
received it is the application's responsibility to decode the message
and construct an `ocpp_message:message()` term. The `ocpp` application
provides library functions in the `ocpp_rpc`, `ocpp_message`, and
`ocpp_error` modules to support this. Once the message is parsed it
can be forwarded to the `ocpp` application using the
`ocpp_station:rpccall/2`, `ocpp_station:rpcreply/2`, or
`ocpp_station:rpcerror/2` function that corresponds to the message
type. For example, using `cowboy`.

```erlang
websocket_handle(Frame = {text, Msg}, State) ->
    case ocpp_rpc:decode(Msg) of
        {ok, {call, _MessageId, OCPPMessage}} ->
            ocpp_station:rpccall(State#state.station, OCPPMessage),
            {ok, State};
        {ok, {callresult, _MessageId, OCPPMessage}} ->
            ocpp_station:rpcreply(State#state.station, OCPPMessage),
            {ok, State};
        {ok, {callerror, _MessageId, OCPPError}} ->
            ocpp_station:rpcerror(State#state, OCPPError),
            {ok, State};
        {error, DecodeError} ->
            %% construct an ocpp error and send the appropriate
            %% CALLERROR message to the station.
            %% ...
    end.
```

When the CSMS needs to send a message to the station (like a reply to
an RPCCALL) it will send an Erlang message to the connected process
with the form `{ocpp, {rpcreply | rpcerror | rpccall, Message}}`. In
the cowboy example these can be handled in the `websocket_info/2`
callback.

```erlang
%% NOTE the ocpp_rpc api shown below is likely to change.
websocket_info({rpcreply, Message}, State) ->
    {[{text, ocpp_rpc:callresult(
                 ocpp_message:id(Message, ocpp_message:to_map(Message)))}],
     State};
%% ...
```

### OCPP layer

The main point to make about the `ocpp` layer is that the stations
must exist (i.e. have been started by the CSMS layer) prior to the
websocket handler attempting to call `ocpp_station:connect/1`.

### CSMS layer

The CSMS layer is much more nebulous than the websocket layer. It
consists of some application that starts processes for the stations
that are supposed to be managed using `ocpp_manager:add_station/3`
(again, this API will probably changed—or at the very least be moved
into the `ocpp` module). This function serves two purposes. First, it
spins up the processes needed to manage the station within the `ocpp`
application. Second, it installs a module implementing the
`ocpp_handler` behavior as the handler for that station. The handler
module is used, via callbacks, to inform the CSMS when OCPP messages
that require a response arrive and to inform the CSMS about certain
important events. The callback api is fairly simple and consists of
just a few required callbacks.

```erlang
-callback init(InitArg :: any()) -> {ok, State :: any()} |
                                    {error, Reason :: any()}.
%% Initialize any internal state the handler will need when responding
%% to requests.

-callback handle_ocpp(MsgType :: ocpp_message:messagetype(),
                      Message :: ocpp_message:message(),
                      State :: any()) ->
    {reply, Message :: ocpp_message:message(), NewState :: any()} |
    {error, Reason :: ocpp_error:error(), NewState :: any()} |
    {noreply, NewState :: any()}.
%% Handle a message sent by the charging station to the CSMS.

-callback handle_info(Info, State :: any()) ->
    {ok, NewState :: any()}
        when Info :: reboot_required |
                     {report_received, ReportId :: integer()} |
                     station_connected |
                     station_disconnected |
                     station_ready.
%% Handle an event other than receipt of an OCPP message.
```

To handle a BootNotificationRequest, for example, you would implement

```erlang
handle_ocpp('BootNotification', Msg, State) ->
    Response = #{
        currentTime => "2023-10-08T19:41:33Z", %% TODO don't hardcode the time
        status => <<"Accepted">>,
        interval => 3600
    }
    {reply, ocpp_message:new_response('BootNotification',
     Response, ocpp_message:id(Msg))}.
```

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
