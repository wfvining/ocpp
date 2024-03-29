-module(ocpp_station_manager).

-behaviour(gen_server).

-export([start_link/1, whereis/1, add_handler/2]).
-export([init/1,
         handle_call/3, handle_cast/2,
         handle_info/2, terminate/2]).

-define(registry(Name), {via, gproc, ?name(Name)}).
-define(name(Name), {n, l, {?MODULE, Name}}).

-record(state, {handler :: undefined | {module(), any()},
                stationid :: binary(),
                handler_crashes = 0 :: non_neg_integer()}).

-spec start_link(StationId :: binary()) -> gen_server:start_ret().
start_link(StationId) ->
    gen_server:start_link(
      ?registry(StationId), ?MODULE, StationId, []).

%% @doc Add a handler that will be used to notify the CSMS of OCPP
%% messages. If the handler init function fails this returns `{error,
%% {init, Reason}}' where `Reason' is the error that was raised.
-spec add_handler(StationId :: binary(),
                  CSMSHandler :: {Module :: module(), InitArg :: any()}) ->
          ok | {error, Reason :: any()}.
add_handler(StationId, CSMSHandler) ->
    gen_server:call(?registry(StationId), {add_handler, CSMSHandler}).

-spec whereis(StationId :: binary()) -> pid() | undefined.
whereis(StationId) ->
    gproc:where(?name(StationId)).

init(StationId) ->
    {ok, #state{stationid = StationId}}.

handle_call({add_handler, {Module, InitArg} = Handler}, _From,
            #state{handler = undefined, stationid = StationId} = State) ->
    case ocpp_handler:add_handler(StationId, Module, InitArg) of
        ok ->
            {reply, ok, State#state{handler = Handler, stationid = StationId}};
        {ErrType, Reason} when ErrType =:= error;
                               ErrType =:= 'EXIT' ->
            {reply, {error, Reason}, State}
    end;
handle_call(Call, _From, State) ->
    logger:warning("Unexpected call ~p", [Call]),
    {noreply, State}.

handle_cast(Cast, State) ->
    logger:warning("Unexpected cast ~p", [Cast]),
    {noreply, State}.

handle_info({gen_event_EXIT, ocpp_handler,
             {'EXIT', {{ocpp_handler_error, Reason}, _}}},
            #state{handler = {Handler, InitArg},
                   handler_crashes = Crashes} = State) ->
    case ocpp_handler:add_handler(
           State#state.stationid, Handler, InitArg, Reason)
    of
        ok ->
            {noreply, State#state{ handler_crashes = Crashes + 1}};
        {ErrType, Reason} when ErrType =:= error;
                               ErrType =:= 'EXIT' ->
            {stop, {error, {reinstall_handler, Reason}}}
    end;
handle_info(Msg, State) ->
    logger:error("in fallback handle_info: ~p", [Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
