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
    TapYawsSup = {tap_yaws_sup,
                        {tap_yaws_sup, start_link, []},
                        permanent, infinity, supervisor, [tap_yaws_sup]},
    TapClientData = {tap_client_data,
                        {tap_client_data, start_link, []},
                        permanent, 5000, worker, [tap_client_data]},
    TapDS = {tap_ds,
                        {tap_ds, start_link, []},
                        permanent, 5000, worker, [tap_ds]},
    UiTestConfig = tap_config:getconfig(ui_test),
    FtpdConfig = tap_config:getconfig(ftpd),
    OpenFlowConfig = tap_config:getconfig(openflow),
    
    Children = defined(lists:flatten([
        TapYawsSup,
        tap_ftpd(UiTestConfig, FtpdConfig, OpenFlowConfig),
        TapClientData,
        TapDS,
        tap_loom(UiTestConfig, FtpdConfig, OpenFlowConfig),
        test_ui(UiTestConfig, FtpdConfig, OpenFlowConfig)
    ])),
    {ok, {{one_for_one, 5, 10}, Children}}.

defined(L) ->
    lists:filter(
        fun(undefined) -> false;
           (_)         -> true
        end, L).

test_ui(disabled, _, _) ->
    undefined;
test_ui(enabled, _, _) ->
    {tap_test_ui, {tap_test_ui, start_link, []},
        permanent, 5000, worker, [tap_test_ui]}.

tap_ftpd(disabled, enabled, disabled) ->
    [{tap_batch,
        {tap_batch, start_link, []},
        permanent, 5000, worker, [tap_batch]},
    {tap_ftpd, {tap_ftpd, start_link, []},
        permanent, 5000, worker, [tap_ftpd]}];
tap_ftpd(enabled, _, _) ->
    undefined;
tap_ftpd(_, disabled, _) ->
    undefined.

tap_loom(disabled, disabled, enabled) ->
    [{tap_loom, {tap_loom, start_link, []},
        permanent, 5000, worker, [tap_loom]},
    {tap_aggr,
        {tap_aggr, start_link, []},
        permanent, 5000, worker, [tap_aggr]}];
tap_loom(enabled, _, _) ->
    undefined;
tap_loom(_, _, disabled) ->
    undefined.
