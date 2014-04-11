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
    FTP =        {tap_ftpd,
                        {tap_ftpd, start_link, []},
                        permanent, 5000, worker, [tap_ftpd]},
    TapClientData = {tap_client_data,
                        {tap_client_data, start_link, []},
                        permanent, 5000, worker, [tap_client_data]},
    TapDS = {tap_ds,
                        {tap_ds, start_link, []},
                        permanent, 5000, worker, [tap_ds]},
    TapAggr = {tap_aggr,
                        {tap_aggr, start_link, []},
                        permanent, 5000, worker, [tap_aggr]},
    TapBatch = {tap_batch,
                        {tap_batch, start_link, []},
                        permanent, 5000, worker, [tap_batch]},
    Children = defined([
        TapYawsSup,
        FTP,
        TapClientData,
        TapDS,
        TapAggr,
        TapBatch,
        test_ui(tap_config:getconfig(ui_test))
    ]),
    {ok, {{one_for_one, 5, 10}, Children}}.

defined(L) ->
    lists:filter(
        fun(undefined) -> false;
           (_)         -> true
        end, L).

test_ui({_, disabled}) ->
    undefined;
test_ui({_, enabled}) ->
    {tap_test_ui, {tap_test_ui, start_link, []},
        permanent, 5000, worker, [tap_test_ui]}.
