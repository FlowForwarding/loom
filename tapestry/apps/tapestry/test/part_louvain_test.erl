%%------------------------------------------------------------------------------
%% Copyright 2014 FlowForwarding.org
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
%%-----------------------------------------------------------------------------

%% @author Erlang Solutions Ltd. <openflow@erlang-solutions.com>
%% @copyright 2014 FlowForwarding.org

%% Unit tests ideas based on https://bitbucket.org/taynaud/python-louvain

-module(part_louvain_test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-include("tapestry/src/part_louvain.hrl").

-define(NUMTEST, 10).

%%------------------------------------------------------------------------------

tap_data_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         {"modularity_allin_is_zero", fun modularity_allin_is_zero/0}
        ,{"modularity_disjoint_clique", fun modularity_disjoint_clique/0}
        ,{"modularity_ring_clique", fun modularity_ring_clique/0}
        ,{"community_graph_nodes", fun community_graph_nodes/0}
        ,{"community_one", fun community_one/0}
        ,{"community_clique", fun community_clique/0}
        ,{"partition_modularity_increase", fun partition_modularity_increase/0}
        ,{"partition_ring_clique", fun partition_ring_clique/0}
        ,{"partition_sample_1000", fun partition_sample_1000/0}
        ,{timeout, 200, {"partition_sample_10000", fun partition_sample_10000/0}}
        ,{timeout, 1000, {"partition_sample_100000", fun partition_sample_100000/0}}
     ]
    }.

setup() ->
    ok.

cleanup(ok) ->
    ok.

%%------------------------------------------------------------------------------

% When every node is in the same community, then modularity is 0
modularity_allin_is_zero() ->
    lists:foreach(
        fun(_) ->
            G = digraph:new(),
            {Neighbors, Edges, Nodes} = random_graph(G, 50, 0.1),
            Communities = communities_from_nodes(one_community, Nodes),
            Modularity = part_louvain:modularity(part_louvain:graph(Communities, Neighbors, Edges)),
            ?assertEqual(0.0, Modularity),
            digraph:delete(G)
        end, lists:seq(1,?NUMTEST)).

% 1. Bartheemy, M. & Fortunato, S. Resolution limit in community detection. Proceedings of the National Academy of Sciences of the United States of America 104, 36-41(2007).
% Disjoint NumClique cliques of size SizeClique has the
% modularity 1 - 1/NumClique
modularity_disjoint_clique() ->
    lists:foreach(
        fun(_) ->
            G = digraph:new(),
            NumClique = random(5,20),
            SizeClique = random(5,20),
            Communities = lists:foldl(
                fun(C, CS) ->
                    Community = C * 1000,
                    {_, _, V} = complete_graph(G, SizeClique, Community),
                    CS ++ communities_from_nodes(Community, V)
                end, [], lists:seq(1, NumClique)),
            Modularity = part_louvain:modularity(
                            part_louvain:graph(
                                Communities,
                                neighbors_from_digraph(G),
                                edges_from_digraph(G))),
            ?assertEqual(
                roundto(1.0 - 1.0/NumClique, 10000),
                roundto(Modularity, 10000)),
            digraph:delete(G)
        end, lists:seq(1,?NUMTEST)).

% NumClique cliques of size SizeClique connected in a ring with a single
% link has modularity 1 - 1/NumClique - NumClique/NumEdges
modularity_ring_clique() ->
    lists:foreach(
        fun(_) ->
            G = digraph:new(),
            NumClique = random(5,20),
            Communities = ring_clique_graph(G, NumClique),
            Edges = edges_from_digraph(G),
            Modularity = part_louvain:modularity(
                            part_louvain:graph(
                                Communities,
                                neighbors_from_digraph(G),
                                Edges)),
            ?assertEqual(
                roundto(1.0 - 1.0/NumClique - NumClique/length(Edges), 10000),
                roundto(Modularity, 10000)),
            digraph:delete(G)
        end, lists:seq(1,?NUMTEST)).

% Nodes in community graph are communities in the original graph
% Total of edge weights in the graphs is the same
community_graph_nodes() ->
    G = digraph:new(),
    {Neighbors, UnweightedEdges, Nodes} = random_graph(G, 50, 0.1),
    Edges = [{E, random(1,100)} || {E, _} <- UnweightedEdges],
    Communities = community_cliques(Nodes, 5),
    GC = community_graph(Communities, Neighbors, Edges),
    CommunitiesCS = communities_set(GC#louvain_graph.communities),
    CommunitiesS = communities_set(Communities),
    % sets are the same
    ?assert(sets:size(sets:union(CommunitiesCS, CommunitiesS)) ==
                sets:size(sets:intersection(CommunitiesCS, CommunitiesS))),
    ?assert(total_weight(Edges) == total_weight(GC#louvain_graph.edges)),
    digraph:delete(G).

% community graph is the same as the original graph if every node
% is in its own community.
community_one() ->
    G = digraph:new(),
    {Neighbors, Edges, Nodes} = random_graph(G, 50, 0.1),
    Communities = [{N, N} || N <- Nodes],
    GC = community_graph(Communities, Neighbors, Edges),
    ?assertEqual(length(Neighbors), length(GC#louvain_graph.neighbors)),
    ?assertEqual(length(Communities), length(GC#louvain_graph.communities)),
    ?assertEqual(length(Edges), length(GC#louvain_graph.edges)),
    digraph:delete(G).

% complete graph of size 2*N split in two has two nodes with N^2 weight
% between them
community_clique() ->
    G = digraph:new(),
    N = 5,
    {Neighbors, Edges, Nodes} = complete_graph(G, N*2),
    Communities = community_cliques(Nodes, 2),
    GC = community_graph(Communities, Neighbors, Edges),
    ?assertEqual(2, length(GC#louvain_graph.neighbors)),
    ?assertEqual(2, length(GC#louvain_graph.communities)),
    lists:foreach(
        fun({{V,V}, Weight}) -> ?assert(N*(N-1)/2 == Weight);
           ({_, Weight}) -> ?assert(N*N == Weight)
        end, GC#louvain_graph.edges),
    digraph:delete(G).

% Modularity increases with each layer of the dendrogram
partition_modularity_increase() ->
    G = digraph:new(),
    {Neighbors, Edges, _Nodes} = random_graph(G, 1000, 0.01),
    Dendrogram = part_louvain:dendrogram(
                                part_louvain:graph([], Neighbors, Edges)),
    digraph:delete(G),
    LGs = Dendrogram#louvain_dendrogram.louvain_graphs,
    ?assert(length(LGs) > 0),
    lists:foldl(
        fun(LG, LastModularity) ->
            Modularity = part_louvain:modularity(LG),
            ?assert(Modularity > LastModularity),
            Modularity
        end, -1.0, lists:reverse(LGs)).

partition_ring_clique() ->
    G = digraph:new(),
    NumClique = random(5,20),
    _Communities = ring_clique_graph(G, NumClique),
    Dendrogram = part_louvain:dendrogram(graph_from_digraph(G)),
    digraph:delete(G),
    CommunitiesPL = part_louvain:communities(Dendrogram),
    ?assertEqual(NumClique, length(CommunitiesPL)),
    % nodes in each community should be from the corresponding clique
    lists:foreach(
        fun({Community, Nodes}) ->
            lists:foreach(
                fun(Node) ->
                    ?assertEqual(Community div 1000, Node div 1000)
                end, Nodes)
        end, CommunitiesPL).

% Nodes in a particular community in level N are together in a
% community in level N+1.
% XXX not tested

partition_sample_1000() ->
    CommunitiesPL = partition_sample_file("../test/sample_1000"),
    ?assertEqual(30, length(CommunitiesPL)).

partition_sample_10000() ->
    CommunitiesPL = partition_sample_file("../test/sample_10000"),
    ?assertEqual(71, length(CommunitiesPL)).

partition_sample_100000() ->
    CommunitiesPL = partition_sample_file("../test/sample_100000"),
    % XXX getting 14253, but data generator says 11243
    ?assertEqual(11243, length(CommunitiesPL)).

%%------------------------------------------------------------------------------

% read sample data file, discover communities, and check community count.
partition_sample_file(Filename) ->
    part_louvain:communities(
        part_louvain:dendrogram(
            graph_from_file(Filename))).

% return Erdős-Rényi graph, binomial graph
random_graph(G, N, P) ->
    random:seed(now()),
    Vertices = lists:seq(1, N),
    lists:foreach(fun(V) -> digraph:add_vertex(G, V) end, Vertices),
    lists:foreach(
        fun({V1, V2}) ->
            case random:uniform() < P of
                true ->
                    digraph:add_edge(G, {V1,V2}, V1, V2, []);
                false ->
                    ok
            end
        end, combinations2(Vertices)),
    {neighbors_from_digraph(G), edges_from_digraph(G), Vertices}.

neighbor(V, V, V2) ->
    V2;
neighbor(V, V1, V) ->
    V1.

complete_graph(G, N) ->
    complete_graph(G, N, 1).

complete_graph(G, N, Base) ->
    Vertices = lists:seq(Base, N + Base - 1),
    lists:foreach(fun(V) -> digraph:add_vertex(G, V) end, Vertices),
    lists:foreach(
        fun({V1, V2}) ->
            digraph:add_edge(G, {V1,V2}, V1, V2, [])
        end, combinations2(Vertices)),
    {neighbors_from_digraph(G), edges_from_digraph(G), Vertices}.

ring_clique_graph(G, NumClique) ->
    SizeClique = random(5,20),
    {SampleNodes, Communities} = lists:foldl(
        fun(C, {NS, CS}) ->
            Community = C * 1000,
            % capture the first node in each community.
            % use these to form the ring.
            {_, _, V = [N|_]} =
                        complete_graph(G, SizeClique, Community),
            {[N | NS], CS ++ communities_from_nodes(Community, V)}
        end, {[], []}, lists:seq(1, NumClique)),
    link_ring(G, SampleNodes),
    Communities.

neighbors_from_digraph(G) ->
    lists:foldl(
        fun(V, L) ->
            NS = lists:foldl(
                fun(E, NL) ->
                    {_, V1, V2, _} = digraph:edge(G, E),
                    [{neighbor(V, V1, V2), E} | NL]
                end, [], digraph:edges(G, V)),
            [{V, NS} | L]
        end, [], digraph:vertices(G)).

graph_from_digraph(G) ->
    part_louvain:graph([], neighbors_from_digraph(G), edges_from_digraph(G)).

% edge weight is 1.0
edges_from_digraph(G) ->
    [{E, 1.0} || E <- digraph:edges(G)].

communities_from_nodes(C, Nodes) ->
    [{N, C} || N <- Nodes].

combinations2(List) ->
    combinations([], 2, List).

combinations(E, 0, _) -> list_to_tuple(E);
combinations(E, N, L) ->
    lists:flatten([combinations([X | E], N - 1, XS) || [X | XS] <- tails(L)]).

tails([]) -> [];
tails(L = [_|R]) ->
    [L | tails(R)].

random(L, H) ->
    random:uniform(H - L + 1) + L - 1.

roundto(V, P) ->
    round(V * P)/P.

link_ring(G, Nodes = [H | T]) ->
    lists:foreach(
        fun({V1, V2}) ->
            digraph:add_edge(G, {V1,V2}, V1, V2, [])
        end, lists:zip(Nodes, T ++ [H])).

communities_set(Communities) ->
    lists:foldl(
        fun({_, C}, S) ->
            sets:add_element(C, S)
        end, sets:new(), Communities).

total_weight(Edges) ->
    lists:foldl(
        fun({_, Weight}, Total) ->
            Total + Weight
        end, 0.0, Edges).

community_graph(Communities, Neighbors, Edges) ->
    GD = part_louvain:graphd(part_louvain:graph(Communities, Neighbors, Edges)),
    part_louvain:graph(part_louvain:community_graph(GD)).

community_cliques(Nodes, Cliques) ->
    {Communities, _} = lists:mapfoldl(
                    fun(Node, Count) ->
                        {{Node, Count rem Cliques}, Count + 1}
                    end, 0, Nodes),
    Communities.

sort_edge(E = {V1,V2}) when V1 > V2 ->
    E;
sort_edge({V1,V2}) ->
    {V2,V1}.

graph_from_file(Filename) ->
    {Time, Ret} = timer:tc(fun() -> graph_from_file_(Filename) end),
    ?debugFmt("~ngraph_from_file(~p) ~f~n", [Filename, Time / 1000000.0]),
    Ret.

graph_from_file_(Filename) ->
    G = digraph:new(),
    {ok, Datafile} = file:read_file(Filename),
    {match, Edges} = re:run(Datafile, "^([0-9]+)\\s*([0-9]+)$",
                                [global, multiline, {capture,[1,2],binary}]),
    UniqueEdges = lists:usort([sort_edge(
                {binary_to_integer(V1,10), binary_to_integer(V2,10)}) ||
                                                        [V1, V2] <- Edges]),
    lists:foreach(
        fun({V1, V2}) ->
            digraph:add_vertex(G, V1),
            digraph:add_vertex(G, V2),
            digraph:add_edge(G, {V1,V2}, V1, V2, [])
        end, UniqueEdges),
    R = graph_from_digraph(G),
    digraph:delete(G),
    R.
