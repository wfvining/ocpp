%%% @doc Server that manages the set of stations this CSMS is
%%% responsible for. This includes monitoring the websocket server.
%%% @end
%%%
%%% Copyright 2023 (c) Will Vining <wfv@vining.dev>

-module(ocpp_manager).

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(LISTENER_NAME, ocppj).

-record(state, {}).

%% @doc
%% Start the server.
%%
%% @param Options is a `proplist' containing the basic configuration
%% options for the websocket server. Valid options are `{wspath,
%% string()}' and `{wsport, 1..65535}' .
%% @end
-spec start_link(Options :: proplists:proplist()) ->
          {ok, Pid :: pid()} |
          {error, Error :: {already_started, pid()}} |
          {error, Error :: term()} |
          ignore.
start_link(Options) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Options, []).

-spec init(Args :: term()) -> {ok, State :: term()} |
          {ok, State :: term(), Timeout :: timeout()} |
          {ok, State :: term(), hibernate} |
          {stop, Reason :: term()} |
          ignore.
init(Options) ->
    process_flag(trap_exit, true),
    {ok, _} = start_ocpp_server(Options),
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
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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

-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
                State :: term()) -> any().
terminate(_Reason, _State) ->
    cowboy:stop_listener(?LISTENER_NAME).

-spec code_change(OldVsn :: term() | {down, term()},
                  State :: term(),
                  Extra :: term()) -> {ok, NewState :: term()} |
          {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec start_ocpp_server(Options :: proplists:proplist()) -> {ok, pid()}.
start_ocpp_server(Options) ->
    BasePath = proplists:get_value(path, Options, "/ocpp"),
    Port = proplists:get_value(port, Options, 3443),
    Dispatch = cowboy_router:compile(
                 [{'_', [{BasePath ++ "/:csname",
                          ocpp_websocket_handler,
                          []}]}]),
    {ok, _} = cowboy:start_clear(
                ?LISTENER_NAME,
                [{port, Port}],
                #{env => #{dispatch => Dispatch}}).
