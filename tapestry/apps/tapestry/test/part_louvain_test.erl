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

-define(NUMTEST, 10).

%%------------------------------------------------------------------------------

tap_data_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         {"allin_is_zero", fun allin_is_zero/0}
        ,{"disjoint_clique", fun disjoint_clique/0}
        ,{"ring_clique", fun ring_clique/0}
     ]
    }.

setup() ->
    ok.

cleanup(ok) ->
    ok.

%%------------------------------------------------------------------------------

% When every node is in the same community, then modularity is 0
allin_is_zero() ->
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
disjoint_clique() ->
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
ring_clique() ->
    lists:foreach(
        fun(_) ->
            G = digraph:new(),
            NumClique = random(5,20),
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

%%------------------------------------------------------------------------------

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

complete_graph(G, N, Base) ->
    Vertices = lists:seq(Base, N + Base - 1),
    lists:foreach(fun(V) -> digraph:add_vertex(G, V) end, Vertices),
    lists:foreach(
        fun({V1, V2}) ->
            digraph:add_edge(G, {V1,V2}, V1, V2, [])
        end, combinations2(Vertices)),
    {neighbors_from_digraph(G), edges_from_digraph(G), Vertices}.

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

edges_from_digraph(G) ->
    [{E, 1} || E <- digraph:edges(G)].

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
