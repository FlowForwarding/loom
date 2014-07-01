%%------------------------------------------------------------------------------
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%%-----------------------------------------------------------------------------
%%
%% @author Infoblox Inc <info@infoblox.com>
%% @copyright 2013 Infoblox Inc
%% @doc Tap Aggregator module

-module(tap_aggr).

-behavior(gen_server).

-export([start_link/0,
         push_qps/0,
         dns_reply/3,
         new_collector/2]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(STATE, tap_aggr_state).
-record(?STATE, {
            dp_qps = dict:new(),
            last_qps_time,
            qps_update_interval,
            qps_timer,
            query_count = 0,
            collectors = dict:new()
        }).

-define(MIN_UPDATE_TIME_MILLIS, 250).
-define(MAX_UPDATE_TIME_MILLIS, 5000).
-define(MAX_QUERY_COUNT_UPDATE, 1000).

-define(TOTAL_QCOUNT, queries).
-define(DP_QCOUNT(Dp), {queries, Dp}).

%------------------------------------------------------------------------------
% API
%------------------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

push_qps() ->
    gen_server:cast(?MODULE, push_qps).

dns_reply(DatapathId, IpAddr, Reply = {_Requester, _Response}) ->
    gen_server:cast(?MODULE, {dns_reply, Reply, DatapathId, IpAddr}).

new_collector(DatapathId, IpAddr) ->
    gen_server:cast(?MODULE, {new_collector, DatapathId, IpAddr}).

%------------------------------------------------------------------------------
% gen_server callbacks
%------------------------------------------------------------------------------

init([])->
    gen_server:cast(?MODULE, start),
    {ok, #?STATE{}}.

handle_call(Msg, From, State) ->
    error({no_handle_call, ?MODULE}, [Msg, From, State]).

handle_cast(start, State) ->
    new_metrics(),
    NewState = qps_timer(
                    State#?STATE{last_qps_time = tap_time:now(),
                    qps_update_interval = qps_update_interval()}),
    {noreply, NewState};
handle_cast(push_qps, State = #?STATE{collectors = Collectors}) ->
    Now = tap_time:now(),
    CollectorStats = [{ofswitch,
                    DatapathId, IpAddr, per_sec(?DP_QCOUNT(DatapathId))} ||
                            {DatapathId, IpAddr} <- dict:to_list(Collectors)],
    NewState = qps_timer(State),
    tap_client_data:qps(?MODULE, per_sec(?TOTAL_QCOUNT), CollectorStats,
                                                    tap_time:universal(Now)),
    {noreply, NewState#?STATE{last_qps_time = Now, query_count = 0}};
handle_cast({dns_reply, Reply, DatapathId, IpAddr},
                            State = #?STATE{query_count = QueryCount}) ->
    tap_ds:ordered_edge(dns_reply_order(Reply)),
    NewState = update_metrics(DatapathId, IpAddr,
                                State#?STATE{query_count = QueryCount + 1}),
    maybe_push_qps(NewState),
    {noreply, NewState};
handle_cast({new_collector, DatapathId, IpAddr},
                                State = #?STATE{collectors = Collectors}) ->
    DPMetric = ?DP_QCOUNT(DatapathId),
    NewCollectors = maybe_new_metric(DatapathId, IpAddr, DPMetric, Collectors),
    {noreply, State#?STATE{collectors = NewCollectors}};
handle_cast(Msg, State) ->
    error({no_handle_cast, ?MODULE}, [Msg, State]).

handle_info(Msg, State) ->
    error({no_handle_info, ?MODULE}, [Msg, State]).

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

%------------------------------------------------------------------------------
% local functions
%------------------------------------------------------------------------------

qps_update_interval() ->
    {seconds, Time} = tap_config:getconfig(qps_max_interval),
    Time.

qps_timer(State = #?STATE{qps_update_interval = After, qps_timer = OldTimer}) ->
    timer:cancel(OldTimer), % ignore errors
    {ok, NewTimer} = timer:apply_after(After*1000, ?MODULE, push_qps, []),
    State#?STATE{qps_timer = NewTimer}.

maybe_push_qps(#?STATE{query_count = QueryCount, last_qps_time = LastUpdate}) ->
    % push update if:
    %   last update was not less than MIN_UPDATE_TIME_MILLIS seconds ago
    %   last update was more than MAX_UPDATE_TIME_MILLIS seconds ago
    %   more than MAX_QUERY_COUNT_UPDATE queries
    Now = tap_time:now(),
    LastUpdateAge = tap_time:diff_millis(Now, LastUpdate),
    DoUpdate = if
        LastUpdateAge < ?MIN_UPDATE_TIME_MILLIS -> false;
        QueryCount > ?MAX_QUERY_COUNT_UPDATE -> true;
        LastUpdateAge > ?MAX_UPDATE_TIME_MILLIS -> true;
        true -> false
    end,
    case DoUpdate of
        true -> push_qps();
        _ -> ok
    end,
    ok.

dns_reply_order({A, B} = R) when A < B ->
    R;
dns_reply_order({A, B}) ->
    {B, A}.

new_metrics() ->
    new_metric(?TOTAL_QCOUNT).

new_metric(Metric) ->
    folsom_metrics:new_spiral(Metric).

update_metrics(DatapathId, IpAddr, State) ->
    % update the metrics for data received from the datapathid.
    folsom_metrics:notify(?TOTAL_QCOUNT, 1),
    DPMetric = ?DP_QCOUNT(DatapathId),
    NewCollectors = maybe_new_metric(DatapathId, IpAddr, DPMetric,
                                                    State#?STATE.collectors),
    folsom_metrics:notify(DPMetric, 1),
    State#?STATE{collectors = NewCollectors}.

% if metric is not in the existing set, create the metrfic and add
% the metric to the existing set.
maybe_new_metric(DatapathId, IpAddr, Metric, Collectors) ->
    case dict:is_key(DatapathId, Collectors) of
        false ->
            new_metric(Metric),
            dict:store(DatapathId, IpAddr, Collectors);
        _ ->
            Collectors
    end.

per_sec(Metric) ->
    proplists:get_value(one, folsom_metrics:get_metric_value(Metric), 0) / 60.
