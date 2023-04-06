%%% @doc CSMS server.
-module(ocpp_csms).

-behaviour(gen_server).

-export([start_link/0]).
-export([boot_request/2]).
-export([init/1, handle_cast/2, handle_call/3, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

-spec start_link() -> gen_server:start_ret().
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Inform the CSMS of a boot notification request.
%%
%% @param StationId The name of the station sending the request.
%% @end
-spec boot_request(
        StationId :: ocpp:station_id(),
        Request :: ocpp_request:boot_notification()) -> ok.
boot_request(StationId, Request) ->
    notify_csms({boot_request, StationId, Request}).

init([]) ->
    {ok, #state{}}.

handle_cast(_, _) ->
    error('not implemented').

handle_call(_, _, _) ->
    error('not implemented').

code_change(_, _, State) ->
    {ok, State}.

notify_csms(Notification) ->
    From = erlang:alias([reply]),
    gen_server:cast(?SERVER, {notify, From, Notification}).
