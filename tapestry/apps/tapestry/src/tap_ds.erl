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
         ordered_edges/1,
         setlimit/2,
         stop_nci/0,
         save/1,
         load/1]).

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
            limits,
            calc_timeout,
            calc_pid = no_process}).

-define(LOGDURATION(F),
                (fun() ->
                    {TinMicro, R} = timer:tc(fun() -> F end),
                    TinSec = TinMicro div 1000000,
                    ?DEBUG("Time ~s:~B ~s: ~B sec",
                                                [?FILE, ?LINE, ??F, TinSec]),
                    R
                end)()).

%------------------------------------------------------------------------------
% API
%------------------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

push_nci() ->
    gen_server:cast(?MODULE, push_nci).

stop_nci() ->
    gen_server:cast(?MODULE, stop_nci).

clean_data() ->
    gen_server:cast(?MODULE, clean_data).

ordered_edge(Edge) ->
    ordered_edges([Edge]).

ordered_edges(Edges) ->
    gen_server:cast(?MODULE, {ordered_edges, Edges}).

save(Filename) ->
    gen_server:call(?MODULE, {save_graph, Filename}, infinity).

load(Filename) ->
    gen_server:call(?MODULE, {load_graph, Filename}, infinity).

setlimit(Limit, Value) ->
    gen_server:call(?MODULE, {setlimit, Limit, Value}).

%------------------------------------------------------------------------------
% gen_server callbacks
%------------------------------------------------------------------------------

init([]) ->
    gen_server:cast(?MODULE, start),
    MaxVertices = tap_config:getconfig(max_vertices),
    MaxEdges = tap_config:getconfig(max_edges),
    MaxCommunities = tap_config:getconfig(max_communities),
    CommSizeLimit = tap_config:getconfig(comm_size_limit),
    CalcTimeout = tap_config:getconfig(nci_calc_time_limit_sec),
    {ok, #?STATE{calc_timeout = CalcTimeout,
                 limits = {MaxVertices, MaxEdges, CommSizeLimit, MaxCommunities}}}.

handle_call({setlimit, Key, Value}, _From,
                                        State = #?STATE{limits = Limits}) ->
    NewLimits = update_limits(Limits, Key, Value),
    {reply, NewLimits, State#?STATE{limits = NewLimits}};
handle_call({save_graph, Filename}, _From,
                                        State = #?STATE{digraph = Digraph}) ->
    Reply = save_graph(Filename, Digraph),
    {reply, Reply, State};
handle_call({load_graph, Filename}, _From,
                                        State = #?STATE{digraph = Digraph}) ->
    digraph:delete(Digraph),
    NewDigraph = load_graph(Filename),
    {reply, ok, State#?STATE{digraph = NewDigraph}};
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
    {noreply, State, hibernate};
handle_cast(push_nci, State = #?STATE{digraph = Digraph,
                                      calc_pid = CalcPid,
                                      calc_timeout = CalcTimeout,
                                      limits = Limits}) ->
    NewState = case calculating(CalcPid) of
        true ->
            ?DEBUG("NCI Calculation already running ~p(~s), skipping this run",
                                    [CalcPid, pid_current_function(CalcPid)]),
            State;
        false ->
            Pid = push_nci(Digraph, digraph:no_vertices(Digraph), Limits),
            nci_watchdog(CalcTimeout),
            State#?STATE{calc_pid = Pid}
    end,
    {noreply, NewState, hibernate};
handle_cast(clean_data, State = #?STATE{
                                    digraph = Digraph,
                                    data_max_age = DataMaxAge}) ->
    DateTime = calendar:universal_time(),
    clean(Digraph, DateTime, DataMaxAge),
    {noreply, State, hibernate};
handle_cast(stop_nci, State = #?STATE{calc_pid = CalcPid}) ->
    stop_nci(CalcPid),
    {noreply, State#?STATE{calc_pid = no_process}};
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

nci_watchdog(infinity) ->
    no_watchdog_timer;
nci_watchdog(Timeout) ->
    {ok, TRef} = timer:apply_after(Timeout * 1000, ?MODULE, stop_nci, []),
    TRef.

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

add_edges(Digraph, Edges)->
    DateTime = calendar:universal_time(),
    [add_edge(Digraph, E, DateTime) || E <- Edges],
    tap_client_data:num_endpoints(
        digraph:no_vertices(Digraph), digraph:no_edges(Digraph), DateTime).

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

push_nci(_Digraph, 0, _Limits) ->
    % no data to process
    no_process;
push_nci(Digraph, _NumVertices, Limits) ->
    Vertices = digraph:vertices(Digraph),
    Edges = [digraph:edge(Digraph, E) || E <- digraph:edges(Digraph)],
    Pid = spawn(
        fun() ->
            ?DEBUG("Starting NCI Calculation"),
            random:seed(now()),
            G = ?LOGDURATION(new_digraph(Vertices, Edges)),
            {NCI, Communities} = nci:compute_from_graph(G, Limits),
            ?LOGDURATION(tap_client_data:nci(NCI, Communities,
                                                calendar:universal_time())),
            ?LOGDURATION(digraph:delete(G))
        end),
    Pid.
            
update_edge(G, V1, V2, Time)->
    % XXX use add_edge(G, {V1,V2}, V1, V2, Time) instead?
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

new_digraph(Vertices, Edges) ->
            G = digraph:new(),
            lists:foreach(fun(X)->
                              digraph:add_vertex(G,X,X)
                          end, Vertices),
            lists:foreach(fun({_, V1, V2, _}) ->
                              digraph:add_edge(G, V1, V2)
                          end, Edges),
            G.

stop_nci(no_process) ->
    ok;
stop_nci(Pid) when is_pid(Pid) ->
    case calculating(Pid) of
        true ->
            ?WARNING("NCI Calculation timeout ~p(~s)~n",
                [Pid, pid_current_function(Pid)]),
            exit(Pid, timeout);
        false ->
            ok
    end.

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
            digraph:add_vertex(G, V1, DateTime),
            digraph:add_vertex(G, V2, DateTime),
            digraph:add_edge(G, V1, V2, DateTime)
        end, Data),
    ?INFO("Load Complete~n"),
    G.
