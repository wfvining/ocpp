-module(ocpp_station_manager).

-behaviour(gen_server).

-export([start_link/2, whereis/1]).
-export([init/1,
         handle_call/3, handle_cast/2,
         handle_info/2, terminate/2]).

-define(registry(Name), {via, gproc, ?name(Name)}).
-define(name(Name), {n, l, {?MODULE, Name}}).

-record(state, {handler :: {module(), any()},
                stationid :: binary()}).

-spec start_link(StationId :: binary(),
                 CSMSHandler :: {Module :: module(), InitArg :: any()}) ->
          gen_server:start_ret().
start_link(StationId, CSMSHandler) ->
    gen_server:start_link(
      ?registry(StationId), ?MODULE, {StationId, CSMSHandler}, []).

-spec whereis(StationId :: binary()) -> pid() | undefined.
whereis(StationId) ->
    gproc:where(?name(StationId)).

init({StationId, {Module, InitArg} = HandlerCallBackModule}) ->
    case ocpp_handler:add_handler(StationId, Module, InitArg) of
        ok ->
            {ok, #state{handler = HandlerCallBackModule,
                        stationid = StationId}};
        {ErrType, Reason} when ErrType =:= error;
                               ErrType =:= 'EXIT' ->
            {stop, {error, {handler, Reason}}}
    end.

handle_call(Call, _From, State) ->
    logger:warning("Unexpected call ~p", [Call]),
    {noreply, State}.

handle_cast(Cast, State) ->
    logger:warning("Unexpected cast ~p", [Cast]),
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
