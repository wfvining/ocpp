%%% @doc This is the primary server used to handle interaction with
%%% ocpp from clients such as CSMS implementations. It's primary task
%%% is the manage adding and removing stations from the application.
%%% @end
%%%
%%% Copyright 2023 (c) Will Vining <wfv@vining.dev>

-module(ocpp_manager).

-behaviour(gen_server).

-export([start_link/0, add_station/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-define(SERVER, ?MODULE).

-record(state, {stations = #{} :: #{binary() => {pid(), reference()}}}).

%% @doc Start the server.
-spec start_link() ->
          gen_server:start_ret().
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Add a station to the application. If successful a new
%% `ocpp_station' state machine and its accompanying processes will be
%% started and `HandlerCallbackModule' will be installed as the event
%% handler of OCPP messages and events that need to be handled by the
%% CSMS. The Pid of the station manager server for thew new station is
%% returned.
-spec add_station(StationName :: binary(),
                  NumEVSE :: pos_integer(),
                  HandlerCallbackModule :: module()) ->
          {ok, StationManagerPid :: pid()} | {error, Reason :: any()}.
add_station(StationName, NumEVSE, HandlerCallbackModule) ->
    gen_server:call(
      ?SERVER,
      {add_station, {StationName, NumEVSE, HandlerCallbackModule}}).

-spec init(Args :: term()) -> {ok, State :: term()} |
          {ok, State :: term(), Timeout :: timeout()} |
          {ok, State :: term(), hibernate} |
          {stop, Reason :: term()} |
          ignore.
init([]) ->
    {ok, #state{}}.

-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
          {reply, Reply :: term(), NewState :: term()} |
          {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
          {reply, Reply :: term(), NewState :: term(), hibernate} |
          {noreply, NewState :: term()} |
          {noreply, NewState :: term(), Timeout :: timeout()} |
          {noreply, NewState :: term(), hibernate} |
          {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
          {stop, Reason :: term(), NewState :: term()}.
handle_call({add_station, {StationName, NumEVSE, HandlerCallbackModule}},
            _From, #state{stations = Stations} = State) ->
    case maps:is_key(StationName, Stations) of
        true ->
            {reply, {error, already_added}, State};
        false ->
            case do_add_station(StationName, NumEVSE, HandlerCallbackModule) of
                {ok, {StationManagerPid, _} = StationManagerInfo} ->
                    {reply, {ok, StationManagerPid},
                     State#state{
                       stations = Stations#{StationName => StationManagerInfo}}};
                {error, _} = Error ->
                    {reply, Error, State}
            end
    end.

do_add_station(StationName, NumEVSE, HandlerCallbackModule) ->
    case ocpp_station_supersup:start_station(
           StationName,
           NumEVSE,
           HandlerCallbackModule)
    of
        {ok, StationManagerPid} ->
            MonRef = monitor(process, StationManagerPid),
            {ok, {StationManagerPid, MonRef}};
        {error, _} = Error ->
            Error
    end.

-spec handle_cast(Request :: term(), State :: term()) ->
          {noreply, NewState :: term()} |
          {noreply, NewState :: term(), Timeout :: timeout()} |
          {noreply, NewState :: term(), hibernate} |
          {stop, Reason :: term(), NewState :: term()}.
handle_cast(_Request, State) ->
    {noreply, State}.

-spec handle_info(Info :: timeout() | term(), State :: term()) ->
          {noreply, NewState :: term()} |
          {noreply, NewState :: term(), Timeout :: timeout()} |
          {noreply, NewState :: term(), hibernate} |
          {stop, Reason :: normal | term(), NewState :: term()}.
handle_info(_Info, State) ->
    {noreply, State}.
