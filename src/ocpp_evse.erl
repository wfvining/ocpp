%%% @doc State machine representating the state of an EVSE
%%% @end
%%%
%%% Copyright (c) 2023 Will Vining <wfv@vining.dev>

-module(ocpp_evse).

-behaviour(gen_statem).

-export([start_link/1]).
-export([callback_mode/0, init/1]).
%% States
-export([unavailable/3, available/3, reserved/3, occupied/3]).

-record(data, {evseid :: pos_integer()}).

start_link(EVSEId) ->
    gen_statem:start_link(?MODULE, EVSEId, []).

init(EVSEId) ->
    {ok, unavailable, #data{evseid = EVSEId}}.

callback_mode() -> state_functions.

unavailable(_, _, _) ->
    error('not implemented').

available(_, _, _) ->
    error('not implemented').

reserved(_, _, _) ->
    error('not implemented').

occupied(_, _, _) ->
    error('not implemented').
