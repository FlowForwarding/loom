-module(tapestry_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    tap_config:refresh(),

    TapYawsSup = {tap_yaws_sup,
                        {tap_yaws_sup, start_link, []},
                        permanent, infinity, supervisor, [tap_yaws_sup]},
    TapClientData = {tap_client_data,
                        {tap_client_data, start_link, []},
                        permanent, 5000, worker, [tap_client_data]},
    TapDS = {tap_ds,
                        {tap_ds, start_link, []},
                        permanent, 5000, worker, [tap_ds]},
    TapCalc = {tap_calc,
                        {tap_calc, start_link, []},
                        permanent, 5000, worker, [tap_calc]},
%   CommsSup = {tap_comms_sup,
%                       {tap_comms_sup, start_link, []},
%                       permanent, infinity, supervisor, [tap_comms_sup]},

    Run = fun(Datasource) ->
        tap_config:is_defined(Datasource, datasources)
    end,
    
    Children = lists:flatten([
        TapYawsSup,
%       CommsSup,
        tap_ftpd(Run(anonymized) orelse Run(logfile)),
        TapClientData,
        TapDS,
        TapCalc,
        tap_loom(Run(packet_in)),
        test_ui(Run(test_ui))
    ]),
    ok = tap_dns:new_cache(),
    {ok, {{one_for_one, 5, 10}, Children}}.

test_ui(true) ->
    {tap_test_ui, {tap_test_ui, start_link, []},
        permanent, 5000, worker, [tap_test_ui]};
test_ui(_) ->
    [].

tap_ftpd(true) ->
    [{tap_batch,
        {tap_batch, start_link, []},
        permanent, 5000, worker, [tap_batch]},
    {tap_ftpd, {tap_ftpd, start_link, []},
        permanent, 5000, worker, [tap_ftpd]}];
tap_ftpd(_) ->
    [].

tap_loom(true) ->
    [{tap_loom, {tap_loom, start_link, []},
        permanent, 5000, worker, [tap_loom]},
    {tap_aggr,
        {tap_aggr, start_link, []},
        permanent, 5000, worker, [tap_aggr]}];
tap_loom(_) ->
    [].
