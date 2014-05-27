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

-module(tap_ds).

-behavior(gen_server).

-export([start_link/0,
         push_nci/0,
         clean_data/0,
         ordered_edge/1,
         ordered_edges/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("tap_logger.hrl").

-define(STATE, tap_ds_state).
-record(?STATE,{
            digraph,
            nci_update_timer,
            clean_timer,
            data_max_age,
            max_vertices}).

%------------------------------------------------------------------------------
% API
%------------------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

push_nci() ->
    gen_server:cast(?MODULE, push_nci).

clean_data() ->
    gen_server:cast(?MODULE, clean_data).

ordered_edge(Edge) ->
    ordered_edges([Edge]).

ordered_edges(Edges) ->
    gen_server:cast(?MODULE, {ordered_edges, Edges}).

%------------------------------------------------------------------------------
% gen_server callbacks
%------------------------------------------------------------------------------

init([]) ->
    MaxVertices = tap_config:getconfig(max_vertices),
    gen_server:cast(?MODULE, start),
    {ok, #?STATE{max_vertices = MaxVertices}}.

handle_call(Msg, From, State) ->
    error({no_handle_call, ?MODULE}, [Msg, From, State]).

handle_cast(start, State) ->
    DataMaxAge = data_max_age(),
    {ok, NCITimer} = interval_timer(nci_min_interval(), push_nci),
    {ok, CleanTimer} = interval_timer(clean_interval(), clean_data),
    {noreply, State#?STATE{digraph = digraph:new(),
                           nci_update_timer = NCITimer,
                           clean_timer = CleanTimer,
                           data_max_age = DataMaxAge}};
handle_cast({ordered_edges, Edges}, State = #?STATE{digraph = Digraph}) ->
    add_edges(Digraph, Edges),
    {noreply, State};
handle_cast(push_nci, State = #?STATE{digraph = Digraph,
                                      max_vertices = MaxVertices}) ->
    push_nci(Digraph, digraph:no_vertices(Digraph), MaxVertices),
    {noreply, State};
handle_cast(clean_data, State = #?STATE{
                                    digraph = Digraph,
                                    data_max_age = DataMaxAge}) ->
    DateTime = calendar:universal_time(),
    clean(Digraph, DateTime, DataMaxAge),
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

interval_timer(IntervalSec, Func) ->
    timer:apply_interval(IntervalSec*1000, ?MODULE, Func, []).

add_edges(Digraph, Edges)->
    DateTime = calendar:universal_time(),
    [add_edge(Digraph, E, DateTime) || E <- Edges],
    tap_client_data:num_endpoints(digraph:no_vertices(Digraph), DateTime).

clean(G, T, MaxAge)->
    OldVertices = lists:filter(
                    fun(X)->
                        {_, TS} = digraph:vertex(G, X),
                        Age = days_to_seconds(calendar:time_difference(TS, T)),
                        Age > MaxAge
                    end, digraph:vertices(G)),
    ?DEBUG("~n**** Cleaning at Time ~p ****~nMaxAge = ~p~nStale Vertices = ~p~n****",[T, MaxAge, OldVertices]),
    digraph:del_vertices(G, OldVertices).

add_edge(G, E, Time)->
    {A, B} = E,
    case A =/= B of
        true ->
            V1 = digraph:add_vertex(G, A, Time),
            V2 = digraph:add_vertex(G, B, Time),
            update_edge(G, V1, V2, Time),
            update_edge(G, V2, V1, Time);
        false -> error
    end.

push_nci(_Digraph, 0, _MaxVertices) ->
    % no data to process
    ok;
push_nci(Digraph, _NumVertices, MaxVertices) ->
    spawn_link(
        fun() ->
            random:seed(now()),
            {NCI, Communities} = nci:compute_from_graph(Digraph, MaxVertices),
            tap_client_data:nci(NCI, Communities, calendar:universal_time())
        end).
            
update_edge(G, V1, V2, Time)->
    Found = lists:filter(
                    fun(X)-> 
                        {_, FV1, FV2, _} = digraph:edge(G, X),
                        (V1 == FV1) and (V2 == FV2)
                    end, digraph:edges(G, V1)),
    case Found of
        [] -> digraph:add_edge(G, V1, V2, Time);
        [E] -> digraph:add_edge(G, E, V1, V2, Time)
    end.

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
