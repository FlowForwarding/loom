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
%% @doc tap module

-module(tap_calc).

-behavior(gen_server).

-export([start_link/0,
         push_nci/0,
         clean_data/0,
         setlimit/2,
         getlimits/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("tap_logger.hrl").

-define(STATE, tap_ds_state).
-record(?STATE,{
            nci_update_timer,
            clean_timer,
            data_max_age,
            limits}).

%------------------------------------------------------------------------------
% API
%------------------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

push_nci() ->
    gen_server:cast(?MODULE, push_nci).

clean_data() ->
    gen_server:cast(?MODULE, clean_data).

setlimit(Limit, Value) ->
    gen_server:call(?MODULE, {setlimit, Limit, Value}).

getlimits() ->
    gen_server:call(?MODULE, getlimits).

%------------------------------------------------------------------------------
% gen_server callbacks
%------------------------------------------------------------------------------

init([]) ->
    gen_server:cast(?MODULE, start),
    MaxVertices = tap_config:getconfig(max_vertices),
    MaxEdges = tap_config:getconfig(max_edges),
    MaxCommunities = tap_config:getconfig(max_communities),
    CommSizeLimit = tap_config:getconfig(comm_size_limit),
    {ok, #?STATE{limits =
                    {MaxVertices, MaxEdges, CommSizeLimit, MaxCommunities}}}.

handle_call(getlimits, _From, State = #?STATE{limits = Limits}) ->
    {MaxVertices, MaxEdges, MaxCommSize, MaxCommunities} = Limits,
    {reply, [{max_vertices, MaxVertices},
             {max_edges, MaxEdges},
             {comm_size_limit, MaxCommSize},
             {max_communities, MaxCommunities}], State};
handle_call({setlimit, Key, Value}, _From,
                                        State = #?STATE{limits = Limits}) ->
    NewLimits = update_limits(Limits, Key, Value),
    % mark the digraph calculation as dirty
    tap_ds:dirty(),
    {reply, NewLimits, State#?STATE{limits = NewLimits}};
handle_call(Msg, From, State) ->
    error({no_handle_call, ?MODULE}, [Msg, From, State]).

handle_cast(start, State) ->
    DataMaxAge = data_max_age(),
    {ok, NCITimer} = interval_timer(nci_min_interval(), push_nci),
    {ok, CleanTimer} = interval_timer(clean_interval(), clean_data),
    {noreply, State#?STATE{nci_update_timer = NCITimer,
                           clean_timer = CleanTimer,
                           data_max_age = DataMaxAge}};
handle_cast(push_nci, State = #?STATE{
            limits = {MaxVertices, MaxEdges, MaxCommSize, MaxCommunities}}) ->
    tap_ds:push_nci(MaxVertices, MaxEdges, MaxCommSize, MaxCommunities),
    {noreply, State};
handle_cast(clean_data, State = #?STATE{data_max_age = DataMaxAge}) ->
    tap_ds:clean_data(DataMaxAge),
    {noreply, State};
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

update_limits({_MaxVertices, MaxEdges, MaxCommSize, MaxCommunities}, max_vertices, V) ->
    {V, MaxEdges, MaxCommSize, MaxCommunities};
update_limits({MaxVertices, _MaxEdges, MaxCommSize, MaxCommunities}, max_edges, V) ->
    {MaxVertices, V, MaxCommSize, MaxCommunities};
update_limits({MaxVertices, MaxEdges, _MaxCommSize, MaxCommunities}, comm_size_limit, V) ->
    {MaxVertices, MaxEdges, V, MaxCommunities};
update_limits({MaxVertices, MaxEdges, MaxCommSize, _MaxCommunities}, max_communities, V) ->
    {MaxVertices, MaxEdges, MaxCommSize, V};
update_limits(Limits, _, _V) ->
    Limits.

interval_timer(IntervalSec, Func) ->
    timer:apply_interval(IntervalSec*1000, ?MODULE, Func, []).

nci_min_interval() ->
    {seconds, Time} = tap_config:getconfig(nci_min_interval),
    Time.

data_max_age() ->
    LongTimeConfig = tap_config:getconfig(data_max_age),
    long_time_to_seconds(LongTimeConfig).

clean_interval() ->
    LongTimeConfig = tap_config:getconfig(clean_interval),
    long_time_to_seconds(LongTimeConfig).

long_time_to_seconds(LongTimeConfig) ->
    days_to_seconds(
        {proplists:get_value(days, LongTimeConfig),
        proplists:get_value(hms, LongTimeConfig)}).

days_to_seconds({D, {H, M, S}}) ->
   (D * 24 * 60 * 60) + (H * 60 * 60) + (M * 60) + S.
