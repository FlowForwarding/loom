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
%% @doc Header file for part_louvain.erl

%% communities - [{Node, Community}]
%% neighbors - [{Node, [{NeighborNode, Edge}]]
%% edges - [{Edge, Weight}]
-record(louvain_graph,
    {communities,
     neighbors,
     edges}).

%% m - weight of all edges
%% weights - dict: Community -> {AllWeights, InWeights}
%%  AllWeights - weights of all edges involving Community
%%  InWeights - weights of all edges within Community
-record(louvain_weights,
    {m,
     weights}).

%% louvain_graphs - list of #louvain_graph{}
-record(louvain_dendrogram,
    {louvain_graphs
    }).
