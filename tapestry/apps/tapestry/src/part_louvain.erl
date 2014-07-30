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
            communitiesd,   % Node -> Community
            neighborsd,     % Node -> [{NeighborNode, Edge}]
            edgesd}).       % Edge -> Weight

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
            Modularity + CW/M -
            math:pow(AW/(M * 2.0), 2)
        end, 0, WeightsD).


%% Build a list of graphs, each an interation of the partitioning.
%% Returns [{Communities, Neighbors, Edges}], head is best partition.
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
    Weights = weights(GD),
    Modularity = modularity(Weights),
    file:write_file("/tmp/part", io_lib:format("starting communities: ~p~n", [lists:sort(dict:to_list(GD#louvain_graphd.communitiesd))])),
    partition(GD, Weights, Modularity, []).

%% ----------------------------------------------------------------------------
%% Local Functions
%% ----------------------------------------------------------------------------

%% repeat the partitioning until there is no significant
%% improvement in the modularity.
partition(GD, Weights, Modularity, L) ->
    {PartitionedGD, NewModularity} = one_level(GD, Weights, Modularity),
    case NewModularity - Modularity < ?MIN_MODULARITY_CHANGE of
        true ->
            L;
        false ->
            % make a graph of the communities (communities are
            % nodes, edges weighted accordingly), and recurse.
            CommunityGD = community_graph(PartitionedGD),
            partition(CommunityGD,
                      weights(CommunityGD),
                      NewModularity,
                    % XXX convert to #louvain_graph{}
                      [PartitionedGD | L])
    end.

%% Partition the #louvain_graphd{}.
%% Returns the partioned #louvain_graphd{} and the new modularity.
%% In the #louvain_graphd{}, the Communities are updated
%% to reflect the paritioning.
one_level(GD0 = #louvain_graphd{}, Weights0 = #louvain_weights{}, Modularity) ->
    {ok, FD} = file:open("/tmp/part", [append]),
    file:write(FD, io_lib:format("edges: ~p~n", [lists:sort(dict:to_list(GD0#louvain_graphd.edgesd))])),
    file:write(FD, io_lib:format("weights: ~p~n", [dict:to_list(Weights0#louvain_weights.weights)])),
    {Modified, NewWeights, NewGD} = dict:fold(
        fun(Node, NodeNeighbors, {Mods, Weights, GD}) ->
            CommunitiesD = GD#louvain_graphd.communitiesd,
            NodeComm = dict_lookup(Node, CommunitiesD),
            {NodeDegree, NeighborCommWeightsD} =
                        neighboring_community_weights(Node, NodeNeighbors, GD),
            file:write(FD, io_lib:format("node: ~p comm weights: ~p~n", [Node, dict:to_list(NeighborCommWeightsD)])),
            LookupCommWeight = fun(Comm) ->
                                   dict_lookup(Comm, NeighborCommWeightsD, 0.0)
                               end,
            DegCTotW = NodeDegree / (Weights#louvain_weights.m * 2.0),
            file:write(FD, io_lib:format("remove node comm: ~p degree ~p comm weight ~p~n", [NodeComm, NodeDegree, LookupCommWeight(NodeComm)])),
            Weights1 = remove_node(NodeComm, NodeDegree, LookupCommWeight(NodeComm), Weights),
            file:write(FD, io_lib:format("weights after remove: ~p~n", [dict:to_list(Weights1#louvain_weights.weights)])),
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
            file:write(FD, io_lib:format("add node comm: ~p degree ~p comm weight ~p~n", [BestComm, NodeDegree, LookupCommWeight(BestComm)])),
            Weights2 = add_node(BestComm, NodeDegree, LookupCommWeight(BestComm), Weights1),
            file:write(FD, io_lib:format("weights after add: ~p~n", [dict:to_list(Weights2#louvain_weights.weights)])),
            file:write(FD, io_lib:format("node: ~p comm: ~p -> ~p~n", [Node, NodeComm, BestComm])),
            {(BestComm /= NodeComm) or Mods, Weights2, GD#louvain_graphd{communitiesd = dict:store(Node, BestComm, CommunitiesD)}}
        end, {false, Weights0, GD0}, GD0#louvain_graphd.neighborsd),
    NewModularity = modularity(NewWeights),
    file:write(FD, io_lib:format("m: ~g, weights: ~p~n", [NewWeights#louvain_weights.m, dict:to_list(NewWeights#louvain_weights.weights)])),
    file:write(FD, io_lib:format("weight sums: ~p~n", [dict:fold(fun(_, {A, B}, {SA, SB}) -> {A + SA, B + SB} end, {0.0,0.0}, NewWeights#louvain_weights.weights)])),
    file:write(FD, io_lib:format("modularity old: ~g, new ~g, modified: ~p~n", [Modularity, NewModularity, Modified])),
    file:write(FD, io_lib:format("communities: ~p~n", [lists:sort(dict:to_list(NewGD#louvain_graphd.communitiesd))])),
    file:close(FD),
    case not Modified orelse
                    NewModularity - Modularity < ?MIN_MODULARITY_CHANGE of
        true ->
            % stop if the modularity is not changing very much
            % or the last pass didn't change anything
            {NewGD, NewModularity};
        false ->
            one_level(NewGD, NewWeights, NewModularity)
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
            case Node == NeighborNode of
                false ->
                    EdgeWeight =
                            dict_lookup(EdgeId, GD#louvain_graphd.edgesd, 1.0),
                    NeighborComm = dict_lookup(NeighborNode,
                                                GD#louvain_graphd.communitiesd),
                    NewWeightsD = 
                        dict:update_counter(NeighborComm, EdgeWeight, WeightsD),
                    {Sum + EdgeWeight, NewWeightsD};
                true ->
                    {Sum, WeightsD}
            end
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

add_community(Node, GD = #louvain_graphd{neighborsd = NeighborsD,
                                         communitiesd = CommunitiesD}) ->
    GD#louvain_graphd{neighborsd = dict:store(Node, [], NeighborsD),
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
                          Community -> case {Node, NeighborNode} of
                                        {N, N} -> EdgeWeight;
                                        _ -> EdgeWeight/2.0
                                    end;
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
