%%%-------------------------------------------------------------------
%% @doc OCPP top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(ocpp_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link(Options) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Options).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init(Options) ->
    %% Initialize the schemas here so the top level supervisor owns the
    %% ETS tables created by Jesse.
    %%
    %% TODO do this in whatever supervisor is responsible for starting
    %%      the cowboy server.
    ok = ocpp_schema:init_schemas(
           filename:join(code:priv_dir(ocpp), "json_schemas")),
    SupFlags = #{strategy => one_for_all,
                 intensity => 1,
                 period => 3600},
    ChildSpecs = [#{id => ocpp_station_sup,
                    start => {ocpp_station_supersup, start_link, []},
                    restart => permanent,
                    type => supervisor,
                    modules => [ocpp_station_supersup]},
                  #{id => ocpp_csms_sup,
                    start => {ocpp_csms_sup, start_link, [Options]},
                    restart => permanent,
                    tupe => supervisor,
                    modules => [ocpp_csms_sup]}],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
