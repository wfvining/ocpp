# OCPP

An implementation of the Open Charge Point Protocol. This is a
prototype implementation of the websocket-based OCPP-j protocol,
version 2.0.1. This application just manages exchange of OCPP messages;
it does not provide an implementation of a CSMS.

## Architecture

The application can host multiple OCPP servers at different ports and
URLs. Starting a new server is described below. The server uses
`cowboy` to handle HTTP requests and websocket communication.

### Starting a Server

To start an OCPP server the application provides the function
`ocpp:start_ocpp_server/2` which takes a callback module and a list of
options. The callback module must implement the `station_handler`
behavior and is responsible for interaction between the OCPP server
and the CSMS or other application. Options are used to specify the URL
and port where the server is going to listen for connections. By
default the server listens on port 3443 at `"/ocpp"`.

```erlang
{ok, NewServer} = ocpp:start_ocpp_server(
                      my_station_handler,
                      [{port, 8080}, {path, "/my/ocpp"}]).
```

Starts a server that listens at
`http://host:8080/my/ocpp/<station-name>` for connections from charging stations.

### The Station Handler Behavior

A CSMS application provides a module implementing the
`station_handler` behavior when starting a new OCPP server. This
behavior has one required callback that is invoked for every valid
OCPP-J RPC message.

```erlang
-spec handle_rpc(Request :: ocpp_rpc:request(), State :: any()) ->
    {reply, Response :: ocpp_rpc:response(), NewState :: any()}.
```

## Build

    $ rebar3 compile
