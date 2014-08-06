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
%% @doc Community detection based on:
%%
%% Ragahavan, Albert, Kumara. Near linear time algorithm to
%% detect community structures in large-scale networks.
%% PHYSICAL REVIEW E 76, 036106 (2007)
%% @end

-module(part_labelprop).

-export([graph/2,
         find_communities/1]).

-include("tap_logger.hrl").

% Make a graph from a digraph suitable for this module to process
% Return {InternalGraphStructure, CleanupFunction}
graph(Vertices, Edges) ->
    {new_digraph(Vertices, Edges),
     fun(G) -> digraph:delete(G) end}.

% Returns:
% {
%   [{Community, [Node]}], % maps community to nodes in the community
%   {
%      [Node], % vertices (i.e., endpoints) in the graph
%      [{Node1, Node2}] % interfactions (i.e., edges) between Nodes
%   {,
%   {
%      [Community], % communities
%      [{Community1, Community2}] % interactions between communities
%   }
% }
find_communities(G)->
    ?DEBUG("Starting NCI Calculation, ~B vertices, ~B edges",
                        [digraph:no_vertices(G), digraph:no_edges(G)]),
    ?LOGDURATION(prop_labels(G)),
    {communities(G), tap_graph(G), tap_community_graph(G)}.

%%
%% WARNING: Recursive function
%%
%% prop_labels(G) function is an internal function called by the
%% externally callable compute(G) function.
%%
%% This function implements exactly the label propagation algorithm detailed in:
%%
%% Ragahavan, Albert, Kumara. Near linear time algorithm to
%% detect community structures in large-scale networks.
%% PHYSICAL REVIEW E 76, 036106 (2007)
%%
%% Input: digraph represpentation of a graph in which every vertex has a 
%%        unique label  
%%
%% Output: digraph represpentation of a graph in which every vertex has a 
%%        label representating community membership.
%%
%% NOTES: This is a recursive function which has stop condition that
%%        no labels where changed on the preveious iteration.
%%        As detailed in the paper above, infinite oscillations are
%%        avoided by uniformaly randomly choosing ties for labelling,
%%        and randomly choosing vertex processing order on every iteration.
%% 
prop_labels(G)->
    %% This first section of code randomizes the vertex procesing
    %% order for each iteration.
    %% A list of vertices is extracted from the graph using
    %% digraph:vertices(...).
    %% The list of vertices shuffled randomly.
    random:seed(),
    V = [Y || {_, Y} <- lists:sort([{random:uniform(), X}
                                        || X <- digraph:vertices(G)])],

    %% The next section of the code uses the lists:foldl(...) function
    %% in the stdlib of the Erlang/OTP.  foldl allows us to pass a
    %% processing function, a start condition/accumulator, and a list.
    %% The processing function is applied to each member (left to right)
    %% of the list and results are accumulated in the accumlator and
    %% returned at the end of the iteraion.

    %% Processing function is applied to every vertex is
    %% fun(Vertex,Acc)->
    %%       {StopCount,GoCount} = Acc,  % <-- this line EXTRACTS the
    %%                                   %     current StopCount and GoCount
    %%                                   %     from Acc
    %%       Result = label_vertex(G,Vertex),
    %%           case Result of
    %%               go -> 
    %%                  {StopCount,GoCount+1};
    %%               stop ->  
    %%                  {StopCount+1,GoCount}
    %%           end
    %% 
    %%  This can be read as:
    %%          attempt to label every vertex (see the label_vertex function)
    %%          if the vertex got relabeled, Result will be "go".  
    %%          if the label did not get relabeled, Result will be "stop"
    %%          depeding on Result, increment either the GoCount or
    %%          the StopCount
    %%  The accumulated {StopCount,GoCount} results will returned to
    %%  the caller of foldl() after all the vertices have been processed
    %%
    %%  The initial conditions of the accumulator is passed to foldl(...)
    %%  as {0,0,G} where G is a hangle to the digraph structure.  Passing G
    %%  into the accumulator is not necessary, we are doing to support the
    %%  stylistic G2 single-assignment notation later to aid in clarity.
    %% 
    %%  The list to run foldl(...) on is passes as V which by this point
    %%  in the code is a randomized list of vertices of the Erlang graph
    %%  passed into prop_labels(...)
    %% 
    RunCond = lists:foldl(fun(Vertex, Acc)->
				  {StopCount, GoCount, G2} = Acc,
				  Result = label_vertex(G, Vertex),
				  case Result of
				      go -> 
					  {StopCount, GoCount + 1, G2};
				      stop ->  
					  {StopCount + 1, GoCount, G2}
				  end
			  end,{0, 0, G}, V),
    {_NewStopCount, NewGoCount, G3} = RunCond,
    ?DEBUG("prop_labels: ~p~n", [RunCond]),
    %% At this point in the code, 
    %%     NewStopCount represents how many vertices were NOT relabeled
    %%      in this iternation and
    %%     NewGoCount represents how many vertices were relabled in
    %%      this iteration

    %% The following conditional ensures that we do another iteration if 
    %% at least one vertex was relabeled.
    %%
    %% The stop condition, as presented in the paper above, is that
    %% EVERY vertex is labeled identically to a majority
    %% of its neighbours.
    %% 
    %% The stop condition is represente as NewGoCount <= 0 == false
    %% and we then return the handle to the newly labeled graph
    %% whose handle is G3.
    %%
    case NewGoCount > 0 of
	true ->
	    prop_labels(G3);
	false -> 
	    G3
    end.

label_vertex(G, Vertex)->
    {_Vert, Label} = digraph:vertex(G, Vertex),
    {NewLabel, _Num} = calc_label(G, Vertex),
    case NewLabel =/= Label of
	true -> 
	    digraph:add_vertex(G, Vertex, NewLabel),
	    go;
	false -> 
	    stop
    end.

calc_label(G, Vertex)->
    N = digraph:out_neighbours(G, Vertex),
    case N =:= [] of
	false ->
	    NL = [digraph:vertex(G, V) || V <- N],           
	    Dict = dict:new(),
	    LC = count_labels(Dict, NL),
	    LLC = dict:to_list(LC),
	    MaxCount = max_count(LLC),
	    Candidates = lists:filter(fun({_, Count}) ->
					      Count == MaxCount end, LLC),
	    choose_label(Candidates);
	true -> {_, Label} = digraph:vertex(G, Vertex),
		{Label, 0}
    end.

count_labels(Dict, [])->
    Dict;
count_labels(Dict, NL)->
    [Head|Rest] = NL,
    {_, Label} = Head,
    NewDict = dict:update_counter(Label, 1, Dict),
    count_labels(NewDict, Rest).

max_count(ListLabelCount)->
    lists:foldl(fun({_, CCount}, MCount) ->
                    case CCount > MCount of
                        true -> CCount;
                        false -> MCount
                    end
		end, 0, ListLabelCount).

choose_label(Candidates) ->
    Num = random:uniform(length(Candidates)),
    {Label, _} = lists:nth(Num, Candidates),
    {Label, length(Candidates)}.

communities(G) ->
    dict:to_list(lists:foldl(
        fun(V, D) ->
            {V, C} = digraph:vertex(G, V),
            dict_append(C, V, D)
        end, dict:new(), digraph:vertices(G))).

% Add V to the list that's the value for K in the dict D.  This
% is a constant time add, as opposed to dict:append which is O(N).
dict_append(K, V, D) ->
    dict:update(K, fun(Old) -> [V | Old] end, [V], D).

% create a graph of connected communities.  Each vertex is a community.
% Returns {[Node], [{Node1, Node2}]}
tap_community_graph(G) ->
    {EndpointsSet, InteractionsSet} = lists:foldl(
        fun(E, {EPs, IAs}) ->
            {_, V1, V2, _} = digraph:edge(G, E),
            {_, C1} = digraph:vertex(G, V1),
            {_, C2} = digraph:vertex(G, V2),
            {
                sets:add_element(C1, sets:add_element(C2, EPs)),
                case C1 == C2 of
                    true -> IAs;
                    false -> sets:add_element(vsort(C1,C2), IAs)
                end
            }
        end, {sets:new(), sets:new()}, digraph:edges(G)),
    {sets:to_list(EndpointsSet), sets:to_list(InteractionsSet)}.

% create a graph of connected nodes.
% Returns {[Node], [{Node1, Node2}]}
tap_graph(G) ->
    {EndpointsSet, InteractionsSet} = lists:foldl(
        fun(E, {EPs, IAs}) ->
            {_, V1, V2, _} = digraph:edge(G, E),
            {
                sets:add_element(V1, sets:add_element(V2, EPs)),
                case V1 == V2 of
                    true -> IAs;
                    false -> sets:add_element(vsort(V1,V2), IAs)
                end
            }
        end, {sets:new(), sets:new()}, digraph:edges(G)),
    {sets:to_list(EndpointsSet), sets:to_list(InteractionsSet)}.

vsort(V1, V2) when V1 > V2 ->
    {V1, V2};
vsort(V1, V2) ->
    {V2, V1}.

new_digraph(Vertices, Edges) ->
    G = digraph:new(),
    lists:foreach(fun(V)-> digraph:add_vertex(G,V,V) end, Vertices),
    lists:foreach(fun({_, V1,V2, _}) -> digraph:add_edge(G, V1,V2) end, Edges),
    G.
