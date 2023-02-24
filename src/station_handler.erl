%%% @doc Handler for incoming OCPP messages on a charger enpoint.
%%%
%%% These messages are not necessarily from the charging station, they
%%% are just received at the charging station's "address" on the CSMS.
%%% The primary responsibility for this server is to parse the OCPP
%%% messages and dispatch the parsed messages to another CSMS component
%%% to be handled.
%%% @end
%%%
%%% Copyright (c) 2023 Will Vining <wfv@vining.dev>

-module(station_handler).

-behaviour(gen_server).

-export([start_link/2, message/2]).
-export([init/1, handle_call/3, handle_cast/2, code_change/3]).

%-callback handle_ocpp_rpc(Action :: any() , State :: any()) -> {reply, Response, NewHandlerState}.

-record(state, {name :: binary(),
                module :: module(),
                handler_state :: any()}).

-spec start_link(HandlerModule :: module(), StationName :: binary()) ->
          gen_server:start_ret().
start_link(HandlerModule, StationName) ->
    gen_server:start_link(?MODULE, [HandlerModule, StationName], []).

-spec message(Handler :: pid(), Message :: binary()) ->
          {reply, Response :: binary()} | stop.
message(Handler, Message) ->
    gen_server:call(Handler, {message, Message}).

init([StationName, _Options]) ->
    {ok, #state{name = StationName}}.

handle_call({message, Message}, _From, State) ->
    case ocpp_rpc:decode(Message) of
        {ok, {call, MessageId, Action}} ->
            {reply, Response, NewState} = dispatch(Action, State),
            {reply, {reply, Response, MessageId}, NewState};
        {error, {Reason, MessageId}} ->
            {reply, {reply, ocpp_rpc:callerror(Reason, MessageId)}, State};
        {error, Reason} ->
            {reply, {reply, ocpp_rpc:callerror(Reason, <<"-1">>)}, State}
    end;
handle_call(_Call, _From, _State) ->
    error('not implemented').

handle_cast(_Cast, _State) ->
    error('not implemented').

code_change(_OldVsn, _NewVsn, State) ->
    {ok, State}.

dispatch(Action, #state{module = HandlerModule,
                        handler_state = HandlerState} = State) ->
    case HandlerModule:handle_ocpp_rpc(Action, HandlerState) of
        %% For now we just use this pattern; however, more will be added.
        {reply, Response, NewHandlerState} ->
            {reply, Response, State#state{ handler_state = NewHandlerState}}
    end.
