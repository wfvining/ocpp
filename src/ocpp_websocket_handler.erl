%%%-------------------------------------------------------------------
%% @doc Websocket connection handler for charging station endpoints.
%% @end
%%
%% Copyright (c) 2023 Will Vining <wfv@vining.dev>
%%%-------------------------------------------------------------------

-module(ocpp_websocket_handler).

-behaviour(cowboy_websocket).

-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2]).

init(Req, State) ->
    CSName = cowboy_req:binding(csname, Req),
    case ocpp_station_db:get_station(CSName) of
        {ok, Station} ->
            case authenticate(Req, ocpp_station_db:id(Station)) of
                true ->
                    init_authenticated(CSName, Req, State);
                false ->
                    {ok, cowboy_req:reply(401, Req), State}
            end;
        notfound ->
            {ok, cowboy_req:reply(404, Req), State}
    end.

authenticate(Req, Station) ->
    case cowboy_req:parse_header(<<"authorization">>, Req) of
        {basic, Station, Password} ->
            ocpp_authentication:verify(Station, Password);
        {basic, _, _} -> false;
        _ -> false
    end.

init_authenticated(StationName, Req, State) ->
    case cowboy_req:parse_header(<<"sec-websocket-protocol">>, Req) of
        undefined ->
            %% Websocket subprotocol must be specified (OCPP 2.0.1: Part 4 § 3.1.2)
            {ok, cowboy_req:reply(400, Req), State};
        Subprotocols ->
            %% For now we only support OCPP 2.0.1
            case lists:keymember(<<"ocpp2.0.1">>, 1, Subprotocols) of
                true ->
                    Req1 = cowboy_req:set_resp_header(
                             <<"sec-websocket-protocol">>, <<"ocpp2.0.1">>, Req),
                    {cowboy_websocket, Req1, StationName, #{compress => true}};
                false ->
                    %% TODO
                    %%
                    %% "If the CSMS does not agree to using one of the
                    %% subprotocols offered by the client, it MUST
                    %% complete the WebSocket handshake with a
                    %% response without a Sec-WebSocket-Protocol
                    %% header and then immediately close the WebSocket
                    %% connection."
                    %%
                    %% (OCPP 2.0.1: Part 4 § 3.2)
                    {ok, cowboy_req:reply(400, Req), State}
            end
    end.

websocket_init(StationName) ->
    error('not implemented').

websocket_handle({text, Msg}, StationPid) ->
    case ocpp_rpc:decode(Msg) of
        {ok, Message} ->
            Response = handle_message(Message, StationPid),
            {reply, {text, Response}, StationPid};
        {error, Error} ->
            {reply, {text, ocpp_rpc:callerror(Error)}, StationPid}
    end.

websocket_info(_, State) ->
    {ok, State}.

handle_message({call, MessageId, {Action, Payload}}, StationPid) ->
    Request = ocpp_request:from_json(Action, Payload),
    case ocpp_station:handle_rpc(StationPid, Request) of
        {reply, Response} ->
            {reply, {text, ocpp_rpc:callresult(MessageId, Response)}, StationPid};
        {error, Reason} ->
            {reply, {text, ocpp_rpc:callerror(Reason, MessageId)}, StationPid}
    end.
