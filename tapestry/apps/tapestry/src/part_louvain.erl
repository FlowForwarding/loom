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

-module(part_louvain).

-export([graph/3,
         graph/1,
         graphd/1,
         community_graph/1,
         weights/1,
         weights/2,
         modularity/1,
         dendrogram/1]).

-define(MIN_MODULARITY_CHANGE, 0.0000001).

-include("part_louvain.hrl").

-record(louvain_graphd, {
            communitiesd,
            neighborsd,
            edgesd}).

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
            Modularity + CW/(M * 2.0) -
            math:pow(AW/(M * 2.0), 2)
        end, 0, WeightsD).


%% Build a list of graphs, each an interation of the partitioning.
%% Returns [{Communities, Neighbors, Edges}], head is best partition.
dendrogram(#louvain_graph{communities = Communities,
                          neighbors = Neighbors,
                          edges = []}) ->
    % no edges, each node is its own community
    [{[{Node, Node} || {Node, _} <- Communities], Neighbors, []}];
dendrogram(#louvain_graph{communities = Communities,
                               neighbors = Neighbors,
                               edges = Edges}) ->
    dendrogram(#louvain_graphd{communitiesd = dict:from_list(Communities),
                                    neighborsd = dict:from_lsit(Neighbors),
                                    edgesd = dict:from_list(Edges)});
dendrogram(GD = #louvain_graphd{}) ->
    % prime the pump by computing the degree and in-degree of the
    % communities in the graph as it stands initially.  Separate
    % out the computation of these values as an optimization since
    % they are also needed to do the paritioning.
    Weights = weights(GD),
    Modularity = modularity(Weights),
    partition(GD, Weights, Modularity, []).

%% ----------------------------------------------------------------------------
%% Local Functions
%% ----------------------------------------------------------------------------

%% repeat the partitioning until there is no significant
%% improvement in the modularity.
partition(GD, Weights, Modularity, L) ->
    {GPartition, NewModularity} = one_level(GD, Weights, Modularity),
    case NewModularity - Modularity < ?MIN_MODULARITY_CHANGE of
        true ->
            L;
        false ->
            partition(GD,
                      community_graph(GPartition),
                      NewModularity,
                      [GPartition | L])
    end.

%% Partition the #louvain_graphd{}.
%% Returns the partioned #louvain_graphd{} and the new modularity.
%% In the #louvain_graphd{}, the Communities are updated
%% to reflect the paritioning.
one_level(GD = #louvain_graphd{}, Weights0 = #louvain_weights{}, Modularity) ->
    GetNodeCommunity = fun(Node) ->
        dict_lookup(Node, GD#louvain_graphd.communitiesd)
    end,
    {Modified, NewWeights} = dict:fold(
        fun(Node, NodeNeighbors, {Mods, Weights}) ->
            NodeComm = GetNodeCommunity(Node),
            {NodeDegree, NeighborCommWeightsD} =
                        neighboring_community_weights(Node, NodeNeighbors, GD),
            LookupCommWeight = fun(Comm) -> dict_lookup(Comm, NeighborCommWeightsD, 0.0) end,
            DegCTotW = NodeDegree / (Weights#louvain_weights.m * 2.0),
            Weights1 = remove_node(NodeComm, NodeDegree, LookupCommWeight(NodeComm), Weights),
            {BestComm, _} = dict:fold(
                fun(Community,
                    CommunityWeight,
                    {TopCommunity, TopModularityGain}) ->
                    {CommunityDegree, _} = dict_lookup(Community, Weights1#louvain_weights.weights, {0,0}),
                    ModularityGain = CommunityWeight - CommunityDegree * DegCTotW,
                    case ModularityGain > TopModularityGain of
                        true -> {Community, ModularityGain};
                        false -> {TopCommunity, TopModularityGain}
                    end
                end, {NodeComm, 0.0}, NeighborCommWeightsD),
            Weights2 = add_node(BestComm, NodeDegree, LookupCommWeight(BestComm), Weights1),
            {(BestComm /= NodeComm) or Mods, Weights2}
        end, {false, Weights0}, GD#louvain_graphd.neighborsd),
    NewModularity = modularity(NewWeights),
    case not Modified orelse
                    NewModularity - Modularity < ?MIN_MODULARITY_CHANGE of
        true ->
            % stop if the modularity is not changing very much
            % or the last pass didn't change anything
            {GD, NewModularity};
        false ->
            one_level(GD, NewWeights, NewModularity)
    end.

remove_node(NodeComm, NodeDegree, CommWeight, Weights = #louvain_weights{}) ->
    Weights#louvain_weights{weights = dict:update(
        NodeComm,
        fun({AllDegree, InDegree}) ->
            {AllDegree - NodeDegree, InDegree - CommWeight}
        end, {-NodeDegree, -CommWeight}, Weights#louvain_weights.weights)}.

add_node(NodeComm, NodeDegree, CommWeight, Weights) ->
    Weights#louvain_weights{weights = dict:update(
        NodeComm,
        fun({AllDegree, InDegree}) ->
            {AllDegree + NodeDegree, InDegree + CommWeight}
        end, {NodeDegree, CommWeight}, Weights#louvain_weights.weights)}.

% weight of all edges to neighbors, and weights to neighboring communities.
% weights to neighboring communities exclude Node
% {NodeDegree, dict: community -> weight to community
neighboring_community_weights(Node, NodeNeighbors, GD) ->
    lists:foldl(
        fun({NeighborNode, EdgeId}, {Sum, WeightsD}) ->
            EdgeWeight = dict_lookup(EdgeId, GD#louvain_graphd.edgesd, 1.0),
            NewWeightsD = case {Node, NeighborNode} of
                {Node, Node} ->
                    WeightsD;
                {_, NeighborNode} -> 
                    NeighborComm = dict_lookup(NeighborNode,
                                            GD#louvain_graphd.communitiesd),
                    dict:update_counter(NeighborComm, EdgeWeight, WeightsD)
            end,
            {Sum + EdgeWeight, NewWeightsD}
        end, {0.0, dict:new()}, NodeNeighbors).


%% Create a new graph based the #louvain_graph{} where the
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

add_community(Node, GD = #louvain_graphd{communitiesd = CommunitiesD}) ->
    GD#louvain_graphd{communitiesd = dict:store(Node, Node, CommunitiesD)}.

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
    NewNeighbor = {NeighborNode, EdgeId},
    dict:update(Node,
        fun(Neighbors) ->
            [NewNeighbor | Neighbors]
        end,
        [NewNeighbor],
        NeighborsD).

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
                    NeighborCommunity = LookupCommunity(NeighborNode),
                    EdgeWeight = LookupWeight(Edge), 
                    [{Community,
                      EdgeWeight,
                      case NeighborCommunity of
                          Community -> EdgeWeight;
                          _ -> 0
                      end
                      } | L2]
                end, L, NodeNeighbors)
        end, [], NeighborsD),
    % aggregate the Weights list by Community
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

%% Sum the weights of all the edges in the graph.  Edges have a weight
%% of at least 1.
sum_weight(#louvain_graphd{edgesd = EdgesD}) ->
    dict:fold(
        fun(_, EdgeWeight, TotalWeight) ->
            TotalWeight + EdgeWeight
        end, 0, EdgesD).
