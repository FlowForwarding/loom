%%%-------------------------------------------------------------------
%%% @copyright (C) 1999-2013, Erlang Solutions Ltd
%%% @author Marc Sugiyama <marc.sugiyama@erlang-solutions.com>
%%% @doc 
%%% Supervisor for simple network exeuctive open flow stats collector
%%% @end
%%%-------------------------------------------------------------------
-module(simple_ne_stats_sup).
-copyright("2013, Erlang Solutions Ltd.").

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).
-export([start_child/2]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    C = simple_ne_stats,
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    {ok, {SupFlags,
            [{C, {C, start_link, []}, temporary, 1000, worker, [C]}]}}.

start_child(Version, DataPathId) ->
    supervisor:start_child(?MODULE, [Version, DataPathId]).
