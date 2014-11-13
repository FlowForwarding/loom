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
         clean_data/1,
         ordered_edge/1,
         ordered_edges/1,
         stop_nci/1,
         dirty/0,
         save/1,
         load/1,
         community_detector/0,
         community_detector/1]).

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
            community_detector,
            dirty,
            calc_timeout,
            calc_pid = no_process}).

%------------------------------------------------------------------------------
% API
%------------------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

push_nci() ->
    gen_server:cast(?MODULE, push_nci).

stop_nci(Pid) ->
    gen_server:cast(?MODULE, {stop_nci, Pid}).

clean_data(MaxAge) ->
    gen_server:cast(?MODULE, {clean_data, MaxAge}).

dirty() ->
    gen_server:cast(?MODULE, dirty).

ordered_edge(Edge) ->
    ordered_edges([Edge]).

ordered_edges(Edges) ->
    gen_server:cast(?MODULE, {ordered_edges, Edges}).

save(Filename) ->
    gen_server:call(?MODULE, {save_graph, Filename}, infinity).

load(Filename) ->
    gen_server:call(?MODULE, {load_graph, Filename}, infinity).

community_detector() ->
    % application:get_env(tapestry, community_detector, part_labelprop).
    application:get_env(tapestry, community_detector, part_louvain).

% check for supported community detection modules
community_detector(CD = part_labelprop) ->
    application:set_env(tapestry, community_detector, CD);
community_detector(CD = part_louvain) ->
    application:set_env(tapestry, community_detector, CD).

%------------------------------------------------------------------------------
% gen_server callbacks
%------------------------------------------------------------------------------

init([]) ->
    gen_server:cast(?MODULE, start),
    CalcTimeout = tap_config:getconfig(nci_calc_time_limit_sec),
    {ok, #?STATE{calc_timeout = CalcTimeout, dirty = true}}.

handle_call({save_graph, Filename}, _From,
                                        State = #?STATE{digraph = Digraph}) ->
    Reply = save_graph(Filename, Digraph),
    {reply, Reply, State};
handle_call({load_graph, Filename}, _From,
                                        State = #?STATE{digraph = Digraph}) ->
    digraph:delete(Digraph),
    NewDigraph = load_graph(Filename),
    {reply, ok, State#?STATE{dirty = true, digraph = NewDigraph}};
handle_call(Msg, From, State) ->
    error({no_handle_call, ?MODULE}, [Msg, From, State]).

handle_cast(start, State) ->
    {noreply, State#?STATE{digraph = digraph:new()}};
handle_cast(dirty, State) ->
    {noreply, State#?STATE{dirty = true}};
handle_cast({ordered_edges, Edges}, State = #?STATE{digraph = Digraph}) ->
    add_edges(Digraph, Edges),
    {noreply, State#?STATE{dirty = true}, hibernate};
handle_cast(push_nci, State = #?STATE{digraph = Digraph,
                                      dirty = Dirty,
                                      calc_pid = CalcPid,
                                      calc_timeout = CalcTimeout}) ->
    NewState = case calculating(CalcPid) of
        true ->
            ?DEBUG("NCI Calculation already running ~p(~s), skipping this run",
                                    [CalcPid, pid_current_function(CalcPid)]),
            State;
        false ->
            case Dirty of
                false ->
                    ?DEBUG("Skipping NCI Calculation, no new data"),
                    State;
                _ ->
                    Pid = push_nci(Digraph, digraph:no_vertices(Digraph)),
                    nci_watchdog(Pid, CalcTimeout),
                    State#?STATE{calc_pid = Pid, dirty = false}
            end
    end,
    {noreply, NewState, hibernate};
handle_cast({clean_data, DataMaxAge}, State = #?STATE{digraph = Digraph}) ->
    DateTime = calendar:universal_time(),
    clean(Digraph, DateTime, DataMaxAge),
    {noreply, State#?STATE{dirty = true}, hibernate};
handle_cast({stop_nci, CalcPid}, State = #?STATE{calc_pid = CalcPid}) ->
    Dirty = case do_stop_nci(CalcPid) of
        stopped ->
            true;
        _ ->
            false
    end,
    {noreply, State#?STATE{dirty = Dirty, calc_pid = no_process}};
handle_cast({stop_nci, _}, State) ->
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

pid_current_function(Pid) ->
    case erlang:process_info(Pid, current_function) of
        undefined -> [];
        {_, {M, F, A}} -> [atom_to_list(M), $:, atom_to_list(F), $/, integer_to_list(A)]
    end.

nci_watchdog(_, infinity) ->
    no_watchdog_timer;
nci_watchdog(Pid, Timeout) ->
    {ok, TRef} = timer:apply_after(Timeout * 1000, ?MODULE, stop_nci, [Pid]),
    TRef.

add_edges(Digraph, Edges)->
    DateTime = calendar:universal_time(),
    [add_edge(Digraph, E, DateTime) || E <- Edges],
    tap_client_data:num_endpoints(
        digraph:no_vertices(Digraph), digraph:no_edges(Digraph), DateTime).

clean(G, T, MaxAge)->
    ?DEBUG("~n**** Cleaning Vertices~n"),
    digraph:del_vertices(G,
        cleaner(
            fun(X) ->
                {_, {TS, _}} = digraph:vertex(G, X),
                TS
            end, T, MaxAge, digraph:vertices(G))),
    ?DEBUG("~n**** Cleaning Edges~n"),
    digraph:del_edges(G,
        cleaner(
            fun(X) ->
                {_, _, _, {TS, _}} = digraph:edge(G, X),
                TS
            end, T, MaxAge, digraph:edges(G))).

cleaner(TSFn, T, MaxAge, List) ->
    Old = lists:filter(
                    fun(X)->
                        TS = TSFn(X),
                        Age = days_to_seconds(calendar:time_difference(TS, T)),
                        Age > MaxAge
                    end, List),
    ?DEBUG("~n**** Cleaning at Time ~p ****~nMaxAge = ~p~nStale Count = ~p~n****",[T, MaxAge, length(Old)]),
    Old.

add_edge(G, {{A, B}, Metadata}, Time) ->
    add_edge(G, {A, B}, Time, Metadata);
add_edge(G, {A, B}, Time) ->
    add_edge(G, {{A, B}, []}, Time).

add_edge(G, {A, B}, Time, Metadata) ->
    case A =/= B of
        true ->
            V1 = digraph:add_vertex(G, A, {Time, Metadata}),
            V2 = digraph:add_vertex(G, B, {Time, Metadata}),
            update_edge(G, V1, V2, {Time, []}),
            update_edge(G, V2, V1, {Time, []});
        false -> error
    end.

push_nci(_Digraph, 0) ->
    % no data to process
    no_process;
push_nci(Digraph, _NumVertices) ->
    Vertices = ?LOGDURATION(digraph:vertices(Digraph)),
    Edges = ?LOGDURATION([digraph:edge(Digraph, E) || E <- digraph:edges(Digraph)]),
    % Read the environment to get the module to use for label propagation.
    % Do this everytime the calculation is done so it can be changed at
    % runtime.
    CommunityDetector = community_detector(),
    Pid = spawn(
        fun() ->
            % wrap in a try/catch so we get stack traces for any errors
            try
                ?DEBUG("Starting NCI Calculation"),
                random:seed(now()),
                {G, CleanupFn} =
                        ?LOGDURATION(CommunityDetector:graph(Vertices, Edges)),
                {Communities, Graph, CommunityGraph} =
                        ?LOGDURATION(CommunityDetector:find_communities(G)),
                CommunitySizes = [{Community, length(Vs)} ||
                                        {Community, Vs} <- Communities],
                NCI = nci:compute_from_communities(CommunitySizes),
                CommunityD =
                    ?LOGDURATION(pivot_communities(Communities, Graph)),
                tap_client_data:nci(NCI,
                                    CommunityD,
                                    CommunitySizes,
                                    CommunityGraph,
                                    calendar:universal_time()),
                CleanupFn(G)
            catch
                T:E ->
                    ?ERROR("Community Detection Error:~n~p:~p~n~p~n",
                                            [T, E, erlang:get_stacktrace()]),
                    error(E)
            end
        end),
    Pid.


% inputs:
%  [{Community, [Endpoint]}],
%  {
%    [Endpoint], [{Endpoint1, Endpoint2}]
%  }
%
% returns:
% {
%   dict (Community ->  [Endpiont])
%   dict (Community ->  [{Endpoint1, Endpoint2])
% }
pivot_communities(Communities, {_Endpoints, Interactions}) ->
    InteractionsD = lists:foldl(
        fun(Interaction = {V1, V2}, D) ->
            dict_append(V1, Interaction,
                dict_append(V2, Interaction, D))
        end, dict:new(), Interactions),
    lists:foldl(
        fun({Community, Endpoints}, {EAD, IAD}) ->
             {dict:store(Community, Endpoints, EAD),
              dict:store(Community,
                        interactions_for_endpoints(InteractionsD, Endpoints),
                        IAD)}
        end, {dict:new(), dict:new()}, Communities).

dict_append(K, V, D) ->
    dict:update(K, fun(V0) -> [V | V0] end, [V], D).

% make a unique list of edges that have at least one end at 
% each of the Endpoints
interactions_for_endpoints(InteractionsD, Endpoints) ->
    Edges = lists:foldl(
        fun(Endpoint, L) ->
            [dict:fetch(Endpoint, InteractionsD) | L]
        end, [], Endpoints),
    lists:usort(lists:flatten(Edges)).

update_edge(G, V1, V2, TimeMetadata) ->
    % XXX use add_edge(G, {V1,V2}, V1, V2, Time) instead?
    Found = lists:filter(
                    fun(X)-> 
                        {_, _, FV2, _} = digraph:edge(G, X),
                        V2 == FV2
                    end, digraph:out_edges(G, V1)),
    case Found of
        [] -> digraph:add_edge(G, V1, V2, TimeMetadata);
        [E] -> digraph:add_edge(G, E, V1, V2, TimeMetadata)
    end.

days_to_seconds({D, {H, M, S}}) ->
   (D * 24 * 60 * 60) + (H * 60 * 60) + (M * 60) + S.

do_stop_nci(no_process) ->
    noop;
do_stop_nci(Pid) when is_pid(Pid) ->
    case calculating(Pid) of
        true ->
            ?WARNING("NCI Calculation timeout ~p(~s)~n",
                [Pid, pid_current_function(Pid)]),
            exit(Pid, timeout),
            stopped;
        false ->
            noop
    end.

% XXX whereis(tap_comms_calculating)
calculating(no_process) ->
    false;
calculating(Pid) when is_pid(Pid) ->
    is_process_alive(Pid).

save_graph(Filename, Digraph) ->
    ?INFO("Saving ~B edges to ~s~n", [digraph:no_edges(Digraph), Filename]),
    Data = lists:map(
        fun(E) ->
            {_, V1, V2, _} = digraph:edge(Digraph, E),
            {V1, V2}
        end, digraph:edges(Digraph)),
    ?INFO("Writing file~n"),
    file:write_file(Filename, io_lib:format("~p.~n", [Data])),
    ?INFO("Write complete~n").

load_graph(Filename) ->
    ?INFO("Loading data from ~s~n", [Filename]),
    {ok, [Data]} = file:consult(Filename),
    ?INFO("Loading ~B edges~n", [length(Data)]),
    G = digraph:new(),
    DateTime = calendar:universal_time(),
    lists:foreach(
        fun({V1, V2}) ->
            digraph:add_vertex(G, V1, {DateTime, []}),
            digraph:add_vertex(G, V2, {DateTime, []}),
            digraph:add_edge(G, V1, V2, {DateTime, []})
        end, Data),
    ?INFO("Load Complete~n"),
    G.
