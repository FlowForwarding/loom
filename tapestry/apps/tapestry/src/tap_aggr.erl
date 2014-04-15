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
         push_qps/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(STATE, tap_aggr_state).
-record(?STATE, {
            last_qps_time,
            qps_update_interval,
            qps_timer,
            query_count = 0
        }).

%------------------------------------------------------------------------------
% API
%------------------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

push_qps() ->
    gen_server:cast(?MODULE, push_qps).

dns_reply(Msg = {_Requester, _Response}) ->
    gen_server:cast(?MODULE, {dns_reply, Msg}).

%------------------------------------------------------------------------------
% gen_server callbacks
%------------------------------------------------------------------------------

init([])->
    gen_server:cast(?MODULE, start),
    {ok, #?STATE{}}.

handle_call(Msg, From, State) ->
    error({no_handle_call, ?MODULE}, [Msg, From, State]).

handle_cast(start, State) ->
    NewState = qps_timer(
                    State#?STATE{last_qps_time = tap_time:now(),
                    qps_update_interval = qps_update_interval()}),
    {noreply, NewState};
handle_cast(push_qps, State = #?STATE{query_count = QueryCount,
                                      last_qps_time = LastUpdate}) ->
    Now = tap_time:now(),
    Interval = tap_time:diff(Now, LastUpdate),
    QPS = QueryCount / Interval,
    NewState = qps_timer(State),
    tap_client_data:qps(QPS, tap_time:universal(Now)),
    {noreply, NewState#?STATE{last_qps_time = Now, query_count = 0}};
handle_cast({dns_reply, Reply}, State = #?STATE{query_count = QueryCount}) ->
    tap_ds:ordered_edge(dns_reply_order(Reply)),
    NewState = State#?STATE{query_count = QueryCount + 1},
    maybe_push_qps(NewState),
    {noreply, NewState};
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
    {qps_max_interval, {seconds, Time}} =
                                    tap_config:getconfig(qps_max_interval),
    Time.

qps_timer(State = #?STATE{qps_update_interval = After, qps_timer = OldTimer}) ->
    timer:cancel(OldTimer), % ignore errors
    {ok, NewTimer} = timer:apply_after(After*1000, ?MODULE, push_qps, []),
    State#?STATE{qps_timer = NewTimer}.

maybe_push_qps(#?STATE{query_count = _QueryCount, last_qps_time = _LastUpdate}) ->
    % XXX some logic - query count > some maximum, time since last update at least some number of seconds
    % push_qps(),
    ok.

dns_reply_order({A, B} = R) when A < B ->
    R;
dns_reply_order({A, B}) ->
    {B, A}.
