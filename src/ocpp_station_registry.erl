-module(ocpp_station_registry).

-export([new/0, delete/0, register/1, unregister/0, lookup_station/1]).

-spec new() -> ok.
new() ->
    ets:new(sid2pid, [set, public, named_table]),
    ets:new(pid2sid, [set, public, named_table]),
    ok.

delete() ->
    ets:delete(sid2pid),
    ets:delete(pid2sid).

%% @doc Register the calling process as `StationId'
-spec register(StationId :: binary()) -> ok | {error, already_registered}.
register(StationId) when is_binary(StationId) ->
    try
        insert_new(sid2pid, {StationId, self()}),
        insert_new(pid2sid, {self(), StationId})
    catch error:already_registered ->
            {error, already_registered}
    end.

%% Raise an error if ets:insert_new/3 returns false.
insert_new(Table, Tuple) ->
    case ets:insert_new(Table, Tuple) of
        true  -> ok;
        false -> error(already_registered)
    end.

%% @doc Unregister the calling process, if it is registered.
-spec unregister() -> ok.
unregister() ->
    case ets:lookup(pid2sid, self()) of
        [{Pid, StationId}] ->
            ets:delete(sid2pid, StationId),
            ets:delete(pid2sid, Pid),
            ok;
        [] ->
            ok
    end.

%% @doc Return the Pid registered for `StationId'.
-spec lookup_station(StationId :: binary()) ->
          {ok, pid()} |
          {error, unregistered}.
lookup_station(StationId) ->
    case ets:lookup(sid2pid, StationId) of
        [{StationId, Pid}] ->
            {ok, Pid};
        [] ->
            {error, unregistered}
    end.
