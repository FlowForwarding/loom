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
%% @author Erlang Solutions Ltd. <openflow@erlang-solutions.com>
%% @copyright 2014 FlowForwarding.org
%%
%% @doc
%% Community detection based on the Louvain algorithm.  Based on a
%% python implementation by Thomas Aynaud.
%%
%% References:
%% Fast unfolding of communities in large networks, Vincent D. Blondel,
%% Jean-Loup Guillaume, Renaud Lambiotte, Etienne Lefebvre -
%% http://arxiv.org/abs/0803.0476
%% Python implementation - https://bitbucket.org/taynaud/python-louvain
%% @end

-define(ASSERTEQUAL(A,B), begin case A == B of true -> ok; false -> error({?FILE, ?LINE, A, not_equal, B}) end end).

-define(SQUARE(A), ((A)*(A))).

-module(part_louvain).

-export([graph/2,
         find_communities/1,
         graph/3,
         graph/1,
         graphd/1,
         community_graph/1,
         weights/1,
         weights/2,
         modularity/1,
         communities/1,
         dendrogram/1]).

-define(MIN_MODULARITY_CHANGE, 0.0000001).

-include("part_louvain.hrl").
-include("tap_logger.hrl").

-record(louvain_graphd, {
            communitiesd,   % Node -> Community
            neighborsd,     % Node -> [{NeighborNode, Edge}]
            edgesd}).       % Edge -> Weight

% Make a #louvain_graph{} from tap data.
graph(Nodes, Edges) ->
    % index edges
    % Edges0: Node -> [Edge]
    % GEdges: [Edge]
    {EdgesD0, GEdges} = lists:foldl(
        fun({_, N1, N2, _}, {D, L}) ->
            Edge = vsort({N1,N2}),
            {
                dict_append(N1, {N2, Edge},
                    dict_append(N2, {N1, Edge}, D)),
                [{Edge, 1.0} | L]
            }
        end, {dict:new(), []}, Edges),
    % remove duplicate edges
    EdgesD = dict:map(
        fun(_, L) ->
            lists:usort(L)
        end, EdgesD0),
    GNeighbors = lists:foldl(
        fun(Node, L) ->
            [{Node, dict_lookup(Node, EdgesD, [])} | L]
        end, [], Nodes),
    {graph([], GNeighbors, lists:usort(GEdges)), fun(_) -> ok end}.

% Partition the graph using the louvain partitioning algorithm, then
% return the data in the format expected by tap code.
%
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
find_communities(G = #louvain_graph{}) ->
    Dendrogram = dendrogram(G),
    CommunitiesPL = communities(Dendrogram),
    PartitionedG = G#louvain_graph{communities = pivot_communities(CommunitiesPL)},
    CommunityGraphD = community_graph(graphd(PartitionedG)),
    {
        CommunitiesPL,
        tap_graph(PartitionedG),
        tap_graph(CommunityGraphD)
    }.

pivot_communities(CommunitiesPL) ->
    lists:foldl(
        fun({Community, Nodes}, L0) ->
            lists:foldl(
                fun(Node, L1) ->
                    [{Node, Community} | L1]
                end, L0, Nodes)
        end, [], CommunitiesPL).

graph(Communities, Neighbors, Edges) ->
    #louvain_graph{
        communities = Communities,
        neighbors = Neighbors,
        edges = Edges}.

graph(GD = #louvain_graphd{}) ->
    #louvain_graph{
        communities = dict:to_list(GD#louvain_graphd.communitiesd),
        neighbors = dict:to_list(GD#louvain_graphd.neighborsd),
        edges = dict:to_list(GD#louvain_graphd.edgesd)}.

graphd(G = #louvain_graph{}) ->
    #louvain_graphd{
        communitiesd = dict:from_list(G#louvain_graph.communities),
        neighborsd = dict:from_list(G#louvain_graph.neighbors),
        edgesd = dict:from_list(G#louvain_graph.edges)}.

weights(GD = #louvain_graphd{}) ->
    weights(sum_weight(GD), community_degrees(GD)).

weights(M, Weights) ->
    #louvain_weights{
        m = M,
        weights = Weights}.

%% Compute the modularity
modularity(G = #louvain_graph{}) ->
    modularity(graphd(G));
modularity(GD = #louvain_graphd{}) ->
    modularity(weights(GD));
modularity(#louvain_weights{m = M, weights = WeightsD}) ->
    dict:fold(
        fun(_, {AW, CW}, Modularity) ->
            Modularity + CW/(2.0*M) - ?SQUARE(AW/(M * 2.0))
        end, 0, WeightsD).

% returns [{Community, [Nodes]}].
communities(#louvain_graph{communities = Communities}) ->
    dict:to_list(lists:foldl(
        fun({Node, Community}, D) ->
            dict_append(Community, Node, D)
        end, dict:new(), Communities));
communities(#louvain_dendrogram{louvain_graphs = Gs}) ->
    [Base | Rest] = lists:reverse(Gs),
    communities(propagate_communities(Base, Rest)).

%% Build a list of graphs, each an interation of the partitioning.
%% Returns [#louvain_graph{}], head is best partition.
dendrogram(G = #louvain_graph{communities = Communities,
                          edges = []}) ->
    % no edges, each node is its own community
    [G#louvain_graph{communities = [{Node, Node} || {Node, _} <- Communities]}];
dendrogram(G = #louvain_graph{}) ->
    dendrogram(graphd(G));
dendrogram(GD = #louvain_graphd{}) ->
    % prime the pump by computing the degree and in-degree of the
    % communities in the graph as it stands initially.  Separate
    % out the computation of these values as an optimization since
    % they are also needed to do the paritioning.
    #louvain_dendrogram{
                louvain_graphs = partition(GD, [])}.

%% ----------------------------------------------------------------------------
%% Local Functions
%% ----------------------------------------------------------------------------

%% propagate community map through dendrogram
propagate_communities(G, Mappings) ->
    CommunitiesD = dict:from_list(G#louvain_graph.communities),
    % create a new community mapping by walking through the mappings
    % and back propagating the communities from the higher modularity graphs
    % to the lower modularity graphs.
    NewCommunitiesD = lists:foldl(
        fun(#louvain_graph{communities = Mapping}, CD) ->
            MappingD = dict:from_list(Mapping),
            dict:fold(
                fun(Node, Community, CD1) ->
                    % the community in the base graph is the key to
                    % the communities map in graph with next higher
                    % modularity.  The value in the graph with the next
                    % higher modularity is the community for that graph.
                    dict:store(Node, dict_lookup(Community, MappingD), CD1)
                end, CD, CD)
        end, CommunitiesD, Mappings),
    G#louvain_graph{communities = dict:to_list(NewCommunitiesD)}.

%% repeat the partitioning until there is no significant
%% improvement in the modularity.
partition(GD, L) ->
    Weights = weights(GD),
    Modularity = modularity(Weights),
    {PartitionedGD, NewModularity} = one_level(GD, Weights, Modularity),
    ?DEBUG("Parition Modularity: ~p -> ~p", [Modularity, NewModularity]),
    % Stop if the modularity didn't change very much or if (due to
    % a buggy implementation) the modularity is greater than 1.0.
    case
        NewModularity - Modularity < ?MIN_MODULARITY_CHANGE orelse
        (NewModularity > 1.0 andalso length(L) > 0)
    of
        true ->
            L;
        false ->
            % make a graph of the communities (communities are
            % nodes, edges weighted accordingly), and recurse.
            validate_graph(PartitionedGD),
            CommunityGD = community_graph(PartitionedGD),
            partition(CommunityGD,
                      [graph(PartitionedGD) | L])
    end.

%% Partition the #louvain_graphd{}.
%% Returns the partioned #louvain_graphd{} and the new modularity.
%% In the #louvain_graphd{}, the Communities are updated
%% to reflect the paritioning.
one_level(GD0 = #louvain_graphd{}, Weights0 = #louvain_weights{}, Modularity) ->
    % XXX make sure the graph is valid
    validate_graph(GD0),
    EdgesD = GD0#louvain_graphd.edgesd,
    {Modified, NewWeights, NewCommunitiesD} = dict:fold(
        fun(Node, NodeNeighbors, {Mods, Weights, CommunitiesD}) ->
            NodeComm = dict_lookup(Node, CommunitiesD),
            {NodeDegree, NeighborCommWeightsD} =
                        neighboring_community_weights(Node,
                                                      NodeNeighbors,
                                                      EdgesD,
                                                      CommunitiesD),
            LookupCommWeight = fun(Comm) ->
                                   dict_lookup(Comm, NeighborCommWeightsD, 0.0)
                               end,
            M = Weights#louvain_weights.m,
            DegCTotW = NodeDegree/?SQUARE(M * 2.0),
            Weights1 = remove_node(NodeComm,
                                   NodeDegree,
                                   LookupCommWeight(NodeComm),
                                   Weights),
            % test node in each neighboring community and find the
            % community that gives the best modularity gain
            {BestComm, _} = dict:fold(
                fun(Community,
                        CommunityWeight,
                        {TopCommunity, TopModularityGain}) ->
                    {CommunityDegree, _} =
                        dict_lookup(Community,
                                    Weights1#louvain_weights.weights,
                                    {0,0}),
                    % k(i,in)/2m - 2*k(i)*sum(tot)/(2m^2)
                    %  C == proposed community
                    %  k(i,in) === CommunityWeight
                    %  k(i) === NodeDegree
                    %  sum(tot) == CommunityDegree
                    % DegCToW = k(i)/(2m^2)
                    ModularityGain =
                            CommunityWeight/(M * 2.0) -
                                2.0 * CommunityDegree * DegCTotW,
                    case ModularityGain > TopModularityGain of
                        true -> {Community, ModularityGain};
                        false -> {TopCommunity, TopModularityGain}
                    end
                end, {NodeComm, 0.0}, NeighborCommWeightsD),
            Weights2 = add_node(BestComm,
                                NodeDegree,
                                LookupCommWeight(BestComm),
                                Weights1),
            CommunitiesD1 = dict:store(Node, BestComm, CommunitiesD),

            % XXX check weights
            compare_weights(Weights2,
                GD0#louvain_graphd{communitiesd = CommunitiesD1}),
            {(BestComm /= NodeComm) or Mods,
             Weights2,
             CommunitiesD1}
        end,
        {false, Weights0, GD0#louvain_graphd.communitiesd},
        GD0#louvain_graphd.neighborsd),
        NewGD = GD0#louvain_graphd{communitiesd = NewCommunitiesD},

    % XXX recompute weights and see if they match the new weights
    compare_weights(NewWeights, NewGD),

    NewModularity = modularity(NewWeights),
    ?DEBUG("OneLevel Modularity: ~p -> ~p", [Modularity, NewModularity]),
    % stop if the communities didn't change, or the modularity did not
    % increase by very much, or (due to a buggy implementation) the
    % modularity is more than 1.0.
    case not Modified orelse
             NewModularity - Modularity < ?MIN_MODULARITY_CHANGE orelse
             NewModularity > 1.0
        of
        true ->
            % stop if the modularity is not changing very much
            % or the last pass didn't change anything
            {NewGD, NewModularity};
        false ->
            one_level(NewGD, NewWeights, NewModularity)
    end.

% Remove 2*CommWeight when edge is copmletely in the community
% self edges are not doubled
remove_node(NodeComm, NodeDegree, CommWeight, Weights = #louvain_weights{}) ->
    Weights#louvain_weights{weights = dict:update(
        NodeComm,
        fun({AllDegree, InDegree}) ->
            {AllDegree - NodeDegree, InDegree - 2.0 * CommWeight}
        end, {-NodeDegree, -2.0 * CommWeight}, Weights#louvain_weights.weights)}.

% Add 2*CommWeight
% self edges are not doubled
add_node(NodeComm, NodeDegree, CommWeight, Weights) ->
    Weights#louvain_weights{weights = dict:update(
        NodeComm,
        fun({AllDegree, InDegree}) ->
            {AllDegree + NodeDegree, InDegree + 2.0 * CommWeight}
        end, {NodeDegree, 2.0 * CommWeight}, Weights#louvain_weights.weights)}.

compare_weights(WA, GD) ->
    WB = weights(GD),
    ?ASSERTEQUAL(WA#louvain_weights.m, WB#louvain_weights.m),
    WeightsDA = lists:sort(lists:filter(fun({_,{0.0,0.0}}) -> false; (_) -> true end, dict:to_list(WA#louvain_weights.weights))),
    WeightsDB = lists:sort(lists:filter(fun({_,{0.0,0.0}}) -> false; (_) -> true end, dict:to_list(WB#louvain_weights.weights))),
    Missing = WeightsDA -- WeightsDB,
    Extra = WeightsDB -- WeightsDA,
    Details = cw_details(GD, Missing ++ Extra),
    ?ASSERTEQUAL({[], [], []}, {Details, Missing, Extra}).

cw_details(_, []) ->
    [];
cw_details(GD, Weights) ->
    lists:map(
        fun({C, _}) ->
            dump_community(C, GD)
        end, Weights).

dump_community(Community, GD) ->
    % nodes in community
    Nodes = [N || {N, C} <- dict:to_list(GD#louvain_graphd.communitiesd),
                                C == Community].
    % [{N, dict_lookup(N, GD#louvain_graphd.neighborsd, [])} || N <- Nodes].


% weight of all edges to neighbors, and weights to neighboring communities.
% weights to neighboring communities exclude Node
% {NodeDegree, dict: community -> weight to community
neighboring_community_weights(Node, NodeNeighbors, EdgesD, CommunitiesD) ->
    lists:foldl(
        fun({NeighborNode, EdgeId}, {Sum, WeightsD}) ->
            EdgeWeight = dict_lookup(EdgeId, EdgesD, 1.0),
            NeighborComm = dict_lookup(NeighborNode, CommunitiesD),
            NewWeightsD = 
                        dict:update_counter(NeighborComm, EdgeWeight, WeightsD),
            case Node == NeighborNode of
                true ->
                    {Sum + 2.0 * EdgeWeight, NewWeightsD};
                false ->
                    {Sum + EdgeWeight, NewWeightsD}
            end
        end, {0.0, dict:new()}, NodeNeighbors).

%% Create a new graph based the #louvain_graphd{} where the
%% Nodes in the new graph are the communities in the old one.
%% Carry the weights into the new graph by summing all of the
%% links connecting nodes in the communities.
community_graph(#louvain_graphd{communitiesd = CommunitiesD0,
                                neighborsd = NeighborsD0,
                                edgesd = EdgesD0}) ->
    GetEdgeWeight = fun(Edge) -> dict_lookup(Edge, EdgesD0, 1.0) end,
    GetNodeCommunity = fun(Node) -> dict_lookup(Node, CommunitiesD0) end,
    % every edge is in the neighbors list twice, once for each end
    % of the edge.  As a consequence, it's only necessary to add the
    % community of the Node (not the neighbors) and the weight is
    % div by 2 because it will be added in twice.  Links from Node -> Node
    % are also listed twice.
    GDC = dict:fold(
        fun(Node0, Node0Neighbors, UCG) ->
            NodeCommunity = GetNodeCommunity(Node0),
            lists:foldl(
                fun({Neighbor, Edge}, UCGn) ->
                    NeighborCommunity = GetNodeCommunity(Neighbor),
                    Weight = GetEdgeWeight(Edge),
                    add_edge(UCGn, NodeCommunity, NeighborCommunity, Weight)
                end,
                add_community(GetNodeCommunity(Node0), UCG),
                Node0Neighbors)
        end,
        #louvain_graphd{communitiesd = dict:new(),
                        neighborsd = dict:new(),
                        edgesd = dict:new()},
        NeighborsD0),
    % remove duplicate neighbors
    GDC#louvain_graphd{
        neighborsd = dict:map(
                        fun(_, Neighbors) ->
                            lists:usort(Neighbors)
                        end, GDC#louvain_graphd.neighborsd)}.

add_community(Node, GD = #louvain_graphd{neighborsd = NeighborsD,
                                         communitiesd = CommunitiesD}) ->
    GD#louvain_graphd{neighborsd = dict_init(Node, NeighborsD),
                      communitiesd = dict:store(Node, Node, CommunitiesD)}.

add_edge(GD = #louvain_graphd{}, Node, NeighborNode, Weight) ->
    EdgeId = edge_id({Node, NeighborNode}),
    GD#louvain_graphd{
        neighborsd = add_neighbor(Node,
                                  NeighborNode,
                                  EdgeId,
                                  GD#louvain_graphd.neighborsd),
        edgesd = add_weight(EdgeId,
                            Weight/2.0,
                            GD#louvain_graphd.edgesd)}.

edge_id(E = {V1, V2}) when V1 < V2 ->
    E;
edge_id({V1, V2}) ->
    {V2, V1}.

add_neighbor(Node, NeighborNode, EdgeId, NeighborsD) ->
    dict_append(Node, {NeighborNode, EdgeId}, NeighborsD),
    dict_append(NeighborNode, {Node, EdgeId}, NeighborsD).

add_weight(Edge, Weight, EdgesD) ->
    dict:update_counter(Edge, Weight, EdgesD).

% Returns dict: Community -> {AllDegree, InDegree}.
% AllDegree is status.degrees in python code
% InDegree is status.internals in python code
community_degrees(#louvain_graphd{communitiesd = CommunitiesD,
                                 neighborsd = NeighborsD,
                                 edgesd = EdgesD}) ->
    LookupCommunity = fun(Node) -> dict_lookup(Node, CommunitiesD) end,
    LookupWeight = fun(Edge) -> dict_lookup(Edge, EdgesD, 1.0) end,
    % Make a list of weights keyed by the communities for every node
    % in the graph.
    Weights = dict:fold(
        fun(Node, NodeNeighbors, L) ->
            Community = LookupCommunity(Node),
            % find the weights of all edges incident on a node,
            % and the weights of edges incident on node in the same
            % community as node
            lists:foldl(
                fun({NeighborNode, Edge}, L2) ->
                    SelfAdjustment = case Node of
                                        NeighborNode -> 2.0;
                                        _ -> 1.0
                                    end,
                    NeighborCommunity = LookupCommunity(NeighborNode),
                    EdgeWeight = LookupWeight(Edge), 
                    [{Community,
                      % edge to self need to be counted twice
                      EdgeWeight*SelfAdjustment,
                      case NeighborCommunity of
                          Community -> EdgeWeight*SelfAdjustment;
                          _ -> 0.0
                      end
                      } | L2]
                end, L, NodeNeighbors)
        end, [], NeighborsD),
    % aggregate the Weights list by Community
    ok = file:write_file("/tmp/bb", io_lib:format("~ncommunity_degrees: ~p~n", [Weights]), [append]),
    lists:foldl(
        fun({C, AW, CW}, D) ->
            dict:update(C, fun({AS, CS}) -> {AS + AW, CS + CW} end, {AW, CW}, D)
        end, dict:new(), Weights).

dict_lookup(Key, Dict) ->
    dict_lookup(Key, Dict, Key).

dict_lookup(Key, Dict, Default) ->
    case dict:find(Key, Dict) of
        error -> Default;
        {ok, Value} -> Value
    end.

% add Key to Dict with [] value if Key is not in Dict.  Otherwise
% do nothing.
dict_init(Key, Dict) ->
    dict:update(Key, fun(Value) -> Value end, [], Dict).

dict_append(Key, Value, Dict) ->
    dict:update(Key, fun(V0) -> [Value | V0] end, [Value], Dict).

%% Sum the weights of all the edges in the graph.  Edges have a weight
%% of at least 1.
%% XXX double weights of edges between nodes, don't double weignts of
%% edges back to same node?
sum_weight(#louvain_graphd{edgesd = EdgesD}) ->
    dict:fold(
        fun(_, EdgeWeight, TotalWeight) ->
            TotalWeight + EdgeWeight
        end, 0.0, EdgesD).

tap_graph(GD = #louvain_graphd{}) ->
    {
        dict:fetch_keys(GD#louvain_graphd.neighborsd),
        dict:fetch_keys(GD#louvain_graphd.edgesd)
    };
tap_graph(G = #louvain_graph{}) ->
    {
        [N || {N, _} <- G#louvain_graph.neighbors],
        [E || {E, _} <- G#louvain_graph.edges]
    }.

vsort({N1,N2}) when N1 > N2 ->
    {N1,N2};
vsort({N1,N2}) ->
    {N2,N1}.

validate_graph(GD = #louvain_graphd{}) ->
    vg_neighbors_unique(GD),
    vg_neighbors_symmetric(GD),
    vg_neighbor_edges(GD),
    vg_nodes_in_community_are_neighbors(GD).

vg_nodes_in_community_are_neighbors(#louvain_graphd{communitiesd = CommunitiesD, neighborsd = NeighborsD}) ->
    % node in community is node in neighbors
    CommunityKeys = sets:from_list(dict:fetch_keys(CommunitiesD)),
    NeighborKeys = sets:from_list(dict:fetch_keys(NeighborsD)),
    ?ASSERTEQUAL(true, sets:is_subset(CommunityKeys, NeighborKeys)).

% neighbors to each vertex are unique, edges are unique
vg_neighbors_unique(#louvain_graphd{neighborsd = NeighborsD}) ->
    lists:foreach(
        fun({_, NeighborEdgeList}) ->
            vg_list_unique([NN || {NN, _} <- NeighborEdgeList]),
            vg_list_unique([E || {_, E} <- NeighborEdgeList])
        end, dict:to_list(NeighborsD)).

% When N2 is a neighbor of N1, then N1 is a neighbor of N2.
vg_neighbors_symmetric(#louvain_graphd{neighborsd = NeighborsD}) ->
    {Left2Right, Right2Left} = dict:fold(
        fun(Node, Neighbors, {L2R, R2L}) ->
            {
                % for {N, [N1, N2]} -> [{N, N1}, {N, N2}]
                [lists:map(fun({NN1, _}) -> {Node, NN1} end, Neighbors) | L2R],
                % for {N, [N1, N2]} -> [{N1, N}, {N2, N}]
                [lists:map(fun({NN2, _}) -> {NN2, Node} end, Neighbors) | R2L]
            }
        end, {[], []}, NeighborsD),
    % Left2Right and Right2Left lists should be the same
    L2RSet = sets:from_list(lists:flatten(Left2Right)),
    R2LSet = sets:from_list(lists:flatten(Right2Left)),
    [] = sets:to_list(sets:subtract(L2RSet, R2LSet)),
    [] = sets:to_list(sets:subtract(R2LSet, L2RSet)).

% edge in neighbors is edge in edges
vg_neighbor_edges(#louvain_graphd{edgesd = EdgesD, neighborsd = NeighborsD}) ->
    NeighborEdges = sets:from_list(
        dict:fold(
            fun(_, Neighbors, ES0) ->
                lists:foldl(
                    fun({_, Edge}, ES1) ->
                        [Edge | ES1]
                    end, ES0, Neighbors)
            end, [], NeighborsD)),
    Edges = sets:from_list(dict:fetch_keys(EdgesD)),
    ?ASSERTEQUAL(true, sets:is_subset(NeighborEdges, Edges) andalso
                        sets:is_subset(Edges, NeighborEdges)).

vg_list_unique(L) ->
    ?ASSERTEQUAL(lists:sort(L), lists:usort(L)).
