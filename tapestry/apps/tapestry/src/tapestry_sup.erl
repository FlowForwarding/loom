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
    {ok, {{one_for_one, 5, 10}, [
        TapYawsSup,
        FTP
    ]}}.
