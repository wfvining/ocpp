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

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, code_change/3]).

-record(state, {name}).

-spec start_link(StationName :: binary(),
                 Options :: proplists:proplist()) -> gen_server:start_ret().
start_link(StationName, Options) ->
    gen_server:start_link(?MODULE, [StationName, Options], []).

init([StationName, _Options]) ->
    {ok, #state{name = StationName}}.

handle_call(_Call, _From, _State) ->
    error('not implemented').

handle_cast(_Cast, _State) ->
    error('not implemented').

code_change(_OldVsn, _NewVsn, State) ->
    {ok, State}.
