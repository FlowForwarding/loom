%%%-------------------------------------------------------------------
%%% @copyright (C) 1999-2013, Erlang Solutions Ltd
%%% @author Marc Sugiyama <marc.sugiyama@erlang-solutions.com>
%%% @doc
%%% Simple network exeuctive open flow stats poller.
%%% @end
%%%-------------------------------------------------------------------
-module(simple_ne_stats).
-copyright("2013, Erlang Solutions Ltd.").

-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(STATE, simple_ne_stats_state).

-include_lib("loom/include/loom_logger.hrl").

-define(DEFAULT_STATS_INTERVAL, 10).
-define(STATS, [flow, table, port, queue, group, meter]).

-record(?STATE, {
    stats_interval, 
    interval_timer,
    of_version,
    datapath_id
}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Version, DatapathId) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Version, DatapathId], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Version, DatapathId]) ->
    StatsInterval = application:get_env(loom, stats_interval_sec, ?DEFAULT_STATS_INTERVAL),
    IntervalTimer = timer:send_interval(StatsInterval * 1000, poll_stats),
    State = #?STATE{stats_interval = StatsInterval,
                    interval_timer = IntervalTimer,
                    of_version = Version,
                    datapath_id = DatapathId},
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(poll_stats, State = #?STATE{of_version = Version,
                                        datapath_id = DatapathId}) ->
    ok = poll_stats(Version, DatapathId),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

poll_stats(Version, DatapathId) ->
    ofs_handler:sync_send_list(DatapathId, stats_msgs(Version)).

stats_msgs(Version) ->
    [].

stats_msg(Version, flow) ->
    of_msg_lib:get_flow_statistics(Version, all, [], []);
stats_msg(Version, table) ->
    of_msg_lib:get_table_stats(Version);
stats_msg(Version, port) ->
    of_msg_lib:get_port_statistics(Version, all);
stats_msg(Version, queue) ->
    of_msg_lib:get_queue_statistics(Version, any, all);
stats_msg(Version, group) ->
    of_msg_lib:get_group_statistics(Version, all);
stats_msg(Version, meter) ->
    of_msg_lib:get_meter_stats(Version, all).

