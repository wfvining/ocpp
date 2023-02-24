%%% @doc Single OCPP server instance.
%%% @end
%%%
%%% Copyright (c) 2023 Will Vining <wfv@vining.dev>

-module(ocpp_server).

-behaviour(gen_server).

-export([start_link/1, start_station_handler/2]).
-export([init/1, handle_call/3, handle_cast/2, code_change/3, handle_continue/2]).

-record(state, {handler_supervisor :: pid()}).

-spec start_link(HandlerModule :: module()) -> {ok, pid()}.
start_link(HandlerModule) ->
    gen_server:start_link(?MODULE, [self(), HandlerModule], []).

-spec start_station_handler(Server :: pid(), StationName :: binary()) ->
          {ok, Pid :: pid()}.
start_station_handler(Server, StationName) ->
    gen_server:call(Server, {start_handler, StationName}).

init([Supervisor, HandlerModule]) ->
    {ok, not_initialized,
     {continue, {start_handler_sup, Supervisor, HandlerModule}}}.

handle_continue({start_handler_sup, Supervisor, HandlerModule}, not_initialized) ->
    {ok, HandlerSupervisor} = supervisor:start_child(
                                Supervisor,
                                #{id => ocpp_handler_sup,
                                  start => {station_handler_sup, start_link, [HandlerModule]},
                                  restart => permanent,
                                  type => supervisor,
                                  modules => [ocpp_handler_sup]}),
    {noreply, #state{handler_supervisor = HandlerSupervisor}}.

handle_call({start_handler, StationName}, _From, State) ->
    {ok, HandlerPid} = station_handler_sup:start_handler(
                         State#state.handler_supervisor, StationName),
    {reply, {ok, HandlerPid}, State}.

handle_cast(_, _) ->
    error('not implemented').

code_change(_, _, State) ->
    {ok, State}.
