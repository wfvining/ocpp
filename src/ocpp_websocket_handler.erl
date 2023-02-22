%%%-------------------------------------------------------------------
%% @doc Websocket connection handler for charging station endpoints.
%% @end
%%
%% Copyright (c) 2023 Will Vining <wfv@vining.dev>
%%%-------------------------------------------------------------------

-module(ocpp_websocket_handler).

-behaviour(cowboy_websocket).

-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2]).

-record(state, {cshandler :: pid(), server :: pid()}).

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
                    {cowboy_websocket, Req1, cowboy_req:bindings(csname, Req)};
                false ->
                    {ok, cowboy_req:reply(400, Req), State}
            end
    end.

websocket_init([Server, StationName]) ->
    {ok, Handler} = ocpp_server:start_station_handler(Server, StationName),
    %% TODO monitor the handler process and close the connection
    %%      if an error occurs.
    %% Ref = erlang:monitor(process, Handler),
    {ok, #state{cshandler = Handler, server = Server}}.

websocket_handle({text, Msg}, State) ->
    case station_handler:message(State#state.cshandler, Msg) of
        {reply, Response} ->
            {reply, {text, Response}, State};
        stop ->
            {stop, State}
        %% {error, Reason} ->
        %%     %% TODO handle errors correctly according to the OCPP spec.
        %%     {reply, }
    end.

websocket_info(_, State) ->
    {ok, State}.
