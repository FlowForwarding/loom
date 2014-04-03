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
    {ok, {{one_for_one, 5, 10}, [
        TapYawsSup
    ]}}.
