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
         caller/1]).

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
            data_max_age}).

%------------------------------------------------------------------------------
% API
%------------------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

push_nci() ->
    gen_server:cast(?MODULE, push_nci).

clean_data() ->
    gen_server:cast(?MODULE, clean_data).

% helper function because API to timer:apply_after doesn't allow
% for calling a function reference, only function by atom.
caller(Fun) ->
    Fun().

%------------------------------------------------------------------------------
% gen_server callbacks
%------------------------------------------------------------------------------

init([]) ->
    gen_server:cast(?MODULE, start),
    {ok, #?STATE{}}.

handle_call(Msg, From, State) ->
    error({no_handle_call, ?MODULE}, [Msg, From, State]).

handle_cast(start, State) ->
    DataMaxAge = data_max_age(),
    {ok, NCITimer} = interval_timer(fun nci_min_interval/0, fun push_nci/0),
    {ok, CleanTimer} = interval_timer(fun clean_interval/0, fun clean_data/0),
    {noreply, State#?STATE{nci_update_timer = NCITimer,
                           clean_timer = CleanTimer,
                           data_max_age = DataMaxAge}};
handle_cast(push_nci, State = #?STATE{}) ->
    tap_ds:push_nci(),
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

interval_timer(IntervalFunc, Func) ->
    % get the interval to wait
    IntervalSec = IntervalFunc(),
    % Function to call the worker function, then schedule ourselves to run
    % again.  Intentionally re-read the interval so we pick up any
    % runtime changes.
    Fun = fun() ->
              Func(),
              interval_timer(IntervalFunc, Func)
          end,
    % schedule ourselves to run
    ?DEBUG("schedule: ~p for ~p sec in future", [Func, IntervalSec]),
    timer:apply_after(IntervalSec*1000, ?MODULE, caller, [Fun]).

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
