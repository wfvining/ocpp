-module(ocpp_station_registry).

%% API for process registration.
-export([register_name/2, unregister_name/1, whereis_name/1, send/2]).
-export([start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, code_change/3, handle_info/2]).

-define(SERVER, ?MODULE).

-spec start_link() -> gen_server:start_ret().
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec register_name(Name :: term(), Pid :: pid()) -> yes | no.
register_name(Name, Pid) ->
    gen_server:call(?SERVER, {register, Name, Pid}).

-spec unregister_name(Name :: term()) -> term().
unregister_name(Name) ->
    gen_server:call(?SERVER, {unregister, Name}).

-spec whereis_name(Name :: term()) -> pid() | undefined.
whereis_name(Name) ->
    try ets:lookup_element(?SERVER, Name, 2) of
        Pid -> Pid
    catch
        error:badarg ->
            undefined
    end.

-spec send(Name :: term(), Message :: term()) -> pid().
send(Name, Message) ->
    case whereis_name(Name) of
        undefined ->
            exit({badarg, {Name, Message}});
        Pid ->
            Pid ! Message,
            Pid
    end.

-spec stop() -> ok.
stop() ->
    gen_server:cast(?SERVER, stop).

init([]) ->
    TRef = ets:new(?SERVER, [protected, set, named_table]),
    {ok, TRef}.

handle_call({register, Name, Pid}, _From, TRef) ->
    Ref = monitor(process, Pid),
    Reply = case ets:insert_new(TRef, [{Name, Pid, Ref}, {Ref, Name}]) of
                true  -> yes;
                false ->
                    demonitor(Ref),
                    no
            end,
    {reply, Reply, TRef};
handle_call({unregister, Name}, _From, TRef) ->
    Ref = ets:lookup_element(TRef, Name, 3),
    demonitor(Ref),
    ets:delete(TRef, Name),
    {reply, ok, TRef}.

handle_info({'DOWN', Ref, process, _Pid, _}, TRef) ->
    Name = ets:lookup_element(TRef, Ref, 2),
    ets:delete(TRef, Name),
    ets:delete(TRef, Ref),
    {noreply, TRef}.

handle_cast(stop, TRef) ->
    {stop, normal, TRef}.

code_change(_, _, TRef) ->
    {ok, TRef}.
