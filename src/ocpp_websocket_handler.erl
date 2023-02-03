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
                    {cowboy_websocket, Req1,
                     [{csname, cowboy_req:bindings(csname, Req)},
                      {ocpp_version, '2.0.1'}]};
                false ->
                    {ok, cowboy_req:reply(400, Req), State}
            end
    end.

websocket_init(State) ->
    %% TODO start a handler process that parses the JSON messages and
    %% interacts with the CSMS application.
    {ok, Handler} = ocpp:start_station_handler(
                      maps:get(csname, State), maps:remove(csname, State)),
    {ok, State#{cshandler => Handler}}.

websocket_handle(Frame = {text, Msg}, State) ->
    {reply, {text, <<"noooo">>}, State}.

websocket_info(_, State) ->
    {ok, State}.
