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
    Datasource = tap_config:getconfig(datasource),
    
    Children = lists:flatten([
        TapYawsSup,
        tap_ftpd(Datasource),
        TapClientData,
        TapDS,
        tap_loom(Datasource),
        test_ui(Datasource)
    ]),
    {ok, {{one_for_one, 5, 10}, Children}}.

test_ui(test_ui) ->
    {tap_test_ui, {tap_test_ui, start_link, []},
        permanent, 5000, worker, [tap_test_ui]};
test_ui(_) ->
    [].

tap_ftpd(Mode) when Mode == anonymized; Mode == logfile ->
    [{tap_batch,
        {tap_batch, start_link, []},
        permanent, 5000, worker, [tap_batch]},
    {tap_ftpd, {tap_ftpd, start_link, []},
        permanent, 5000, worker, [tap_ftpd]}];
tap_ftpd(_) ->
    [].

tap_loom(packet_in) ->
    [{tap_loom, {tap_loom, start_link, []},
        permanent, 5000, worker, [tap_loom]},
    {tap_aggr,
        {tap_aggr, start_link, []},
        permanent, 5000, worker, [tap_aggr]}];
tap_loom(_) ->
    [].
