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
%% @doc Tapestry.  A Network Complexity Index Calculator
%%
%% This code takes extensive advantage of several Erlang modules
%% which ship with the Erlang/OTP
%%
%%        -module(digraph): http://www.erlang.org/doc/man/digraph.html
%%        -module(lists): http://www.erlang.org/doc/man/lists.html
%%        -module(dict): http://www.erlang.org/doc/man/dict.html
%%        -module(random): http://www.erlang.org/doc/man/random.html
%%
%% Stylistically we maintain an "single-assignment" functional style
%% even when dealing with handles to data structure
%% which could be modified opaquely.
%% 
%% For example, because the "digraph" module introduces modifiable
%% global data structures, the code snippet:
%%
%%          G = digraph:new(),
%%          process(G),
%%
%% would imply that after "process(G)", G refers to a modified data
%% structure which we COULD then pass to another function:
%%
%%          process2(G),
%%          
%% However, in this code we CHOOSE to maintain the single-assignment
%% style to be consistent with the single-assignement handling of native
%% data structures in Erlang.
%%    
%% Therefore, we CHOOSE to write the above in a single-assigment style this way:
%%
%%          G = digraph:new(),
%%          G2 = process(G),
%%          G3 = process2(G2),
%%  
%% This keeps the code a little more syntaxically consistent with
%% with the single-assigment behavior of native Erlang data structures
%% like simple variables and lists.  We will note where we intentionally
%% deviate from this approach.

-module(nci).

-export([compute/1,
         compute_from_graph/2,
         clean_vertex/2,
         print_labels/1]).

%% === compute(EdgeList) === 
%%
%% compute(EdgeList) is the API function to be called by external calling code.
%%
%% Input: an Erlang list representation of undirected edges of a graph
%%
%% Output: The Network Complexit Index (NCI) for the input graph as described
%%         in Bailey, Grossman. A Network Complexity Index for Networks
%%         of Networks. 2013
%%         http://www.flowforwarding.org/docs/Bailey%20-%20Grossman%20article%20on%20network%20complexity.pdf
%%
%% The input should be in the form [{VERTEX, VERTEX}, {VERTEX,VERTEX}]
%% Examples:
%%          [{a,b},{a,c},{a,d},{a,d}] represents a graph with four
%%          vertices: a, b, c, d and three edges: one edge between a and b,
%%          one betwen a and c, and one between a and d.
%%
%% Notes:
%%        Sizes of up to a million edges should run fast on modern laptop
%%        hardware.
%%
%%        Duplicate edges or directed representations in the input
%%        will be converted to an undirected, unweighted graph before the
%%        label propagation step.  
%%        For example, an input of [ {a,b}, {b,a}, {b,a} ] will result in 
%%        A graph with single undiretected edge between a and b.
%%
%%        Vertex labels can be any legal Erlang number or atom
%%        http://www.erlang.org/doc/reference_manual/data_types.html
%%

-spec compute(EdgeList :: list()) -> NCI :: integer().
compute(EdgeList)->
    %%  Convert the list of edges into an Erlang "digraph"
    G = digraph:new(),
    G2 = add_edges(G,EdgeList),
 
    %%  At this point G2 is a handle to an Erlang data structure which
    %%  represents an Erlang digraph which was expressed in the passed
    %%  EdgeList parameter including duplicate edges and directional edges
    %%  where {a,b} means an edge from a to b
    %%
    %%  Conveniently the Erlang digraph data structure 
    %%  allows one "label" to be attached to each vertex.
  
    %%  Next we preprocess the digraph G to
    %%  ensure all edges between two vertices have 
    %%  exactly one in_edge and one_out edge.
    %%
    %%  NOTE TO RESEARCHERS:
    %%  The decision to preprocess the graph may effect
    %%  convergence time of the label propegation algorithm
    %%  and may effect the community detection itself
    %%  further research is required in this area.  We 
    %%  decided to do this preprocessing in order to 
    %%  directly implement the label propagation algorithm
    %%  in described the paper cited below.
    %%  
    V = digraph:vertices(G2),

    %%  At this point, V is an immuntable list of vertices in the graph.
    %%  We then pass this list to the "list comprehension" syntax 
    %%  call to "clean_vertex" below.
    %%  You can read more about "list comprehension" here:
    %%  http://www.erlang.org/doc/programming_examples/list_comprehensions.html
    %% 
    %%  You can read the statement below as
    %%  "for every digraph vertex Vertex in the list V, call
    %%  clean_vertex(G2,Vertex)"
    %%
    [clean_vertex(G2,Vertex) || Vertex <- V],

    %%  In the above list comphrehension we exploit the fact that 
    %%  G2 is actually just a handle and not an immutable
    %%  data structure. 
    %% 
    %%  At this point in the code G2 now points to a
    %%  preprocessed digraph on which we want to
    %%  call label progragation algorithm.
    %%  Call the label propogation algorithm on G2
    G3 = prop_labels(G2),
    
    %% At this point G3 is a handle pointing to a digraph datastructure in
    %% which every label of every vertex denotes the community membership 
    %% of the vertex.
    %%
    %% Again we choose to declare G3 for stylistic reasons; at this point 
    %% in the code, G2 points to the same data structure.
    
    NCI = calc_nci(G3),
    digraph:delete(G),

    %% Return the NCI number.
    NCI.

compute_from_graph(G, MaxVertices)->
    prop_labels(G),
    NCI = calc_nci(G),
    % !!! warning, communities mangles G
    Communities = communities(G, MaxVertices),
    {NCI, Communities}.

%% === calc_nci ===
%% function is an internal function called by the externally
%% callable compute(G) function
%%
%% Input: an Erlang digraph representation of a graph in which every
%%        vertex has a %label representating community membership.  
%%
%%        The "prop_label" function below is one implementation of
%%        community detection which outputs a properly labeled graph
%%
%% Output: The Network Complexity Index for the input graph described in
%%         Bailey, Grossman. A Network Complexity Index for Networks of
%%         Networks. 2013
%%         http://www.flowforwarding.org/docs/Bailey%20-%20Grossman%20article%20on%20network%20complexity.pdf
%%
-spec calc_nci( LabeledGraph :: digraph() ) -> NCI :: integer().
calc_nci(LabeledGraph)->
    C = get_communities(LabeledGraph),

    %% At this point in the code C refers to an unordered of community
    %% names with associated sizes
    %% For example if C = [{a,10},{f,3},{c,55}], it is a representation
    %% of three communities a, f, and c
    %% which have 10, 3, and 55 members respectively
    
    SC = rev_sort_labels(C),

    %% At this point, SC referes to a sorted list of communities with
    %% sizes which have been sorted by size
    %% such that the largest communties are at the head of the list.
    %% Using the example C above,  SC = [{c,55},{a,10},{f,3}].

    %% We now call a two parameter function 
    NCI = calc_nci(SC,0),
    %% NCI computation results in a one to big, maybe?
    %% XXX requires more study.
    NCI.

%%
%% WARNING: Recursive function
%%
%% calc_nci(SortedList,NCI) is a recursive internal function called by
%% the non-recursive calc_nci(LablelGraph) function above
%%
%% This is a simple algorithm which just recursively walks a sorted
%% list of {community,size} elements and returns the balance point
%% between sizes and and numbers of the communities.
%%
%% For example the input list of
%% [{b,100},{c,100},{a,50},{z,43},{f,20},{g,3},{x,3},{p,2}]
%% returns 5: "There are exactly 5 communties that are 
%% greater than or equal to 5 in size." 
%%
%% Input: A sorted list of communities with sizes which have been
%%        sorted by size such that the largest communties are at the
%%        head of the list. See example above.
%%
%% Output: The Network Complexit Index for the input graph described in
%%         Bailey, Grossman. A Network Complexity Index for Networks of
%%         Networks. 2013
%%         http://www.flowforwarding.org/docs/Bailey%20-%20Grossman%20article%20on%20network%20complexity.pdf
%%
-spec calc_nci(SortedList :: list(), NCI :: integer()) -> NCI :: integer().
calc_nci([], NCI)->
    NCI;
calc_nci(SortedList, NCI) ->
    [Head|Rest] = SortedList,
    {_,Size} = Head,
    case (NCI >= Size) of
	true ->
	    NCI;
	false ->
	    calc_nci(Rest, NCI+1)
    end.

%%
%% get_communties(G) function is an internal function called by the
%% calc_nci(LabeledGraph) function.
%%
%% Input: Erlang digraph represpensation of a graph in which every vertex has a 
%%        label representating community membership.  
%%
%%        The "prop_label" function below is one implementation of
%%        community detection which outputs a properly labeled graph
%%
%% Output: a unsorted list of {community,size} elements.
%%        For example an output of [{a,10},{f,3},{c,55}] is a representation
%%        of three communities a, f, and c which have 10, 3, and 55 members
%%        respectively 
%%
get_communities(G)->
    Vertices = [digraph:vertex(G,V) || V <- digraph:vertices(G)],
    Dict = dict:new(),
    C = count_labels(Dict, Vertices),
    CL = dict:to_list(C),
    CL.
    

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
%% Input: digraph represpensation of a graph in which every vertex has a 
%%        unique label  
%%
%% Output: digraph represpensation of a graph in which every vertex has a 
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
    %% The list of vertices is randomly split and the two section
    %% concatenated back together by placing the tail portion of
    %% the list in front of the head portion of the list
    random:seed(),
    Vertices = digraph:vertices(G),
    SplitValue = random:uniform(length(Vertices)),
    {V1, V2} = lists:split(SplitValue, Vertices),
    V = V2 ++ V1,

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
	    NL = [ digraph:vertex(G, V) || V <- N],
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

%%% ======== UTILS ==== %%%

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
    
print_labels(G)->
   io:format("~p~n",[[digraph:vertex(G,V) || V <- digraph:vertices(G) ]]).

rev_sort_labels(List)->
    Sorted = lists:sort(fun({_, Label1},{_, Label2})->
				Label1 > Label2 end, List),
    Sorted.

clean_vertex(G, Vertex)->
    N = digraph:out_neighbours(G, Vertex),
    NL = [digraph:vertex(G,V) || V <- N],
    SortedNL = lists:keysort(1, NL),
    clean_neighbours(G, Vertex, SortedNL).

clean_neighbours(G, V, SortedNL)->
    Dirt = clean_neighbours(G, V, SortedNL, {}, []),
    remove_dirt(G, V, Dirt).

clean_neighbours(_G, _V, [], _P,Acc)->
    Acc;
clean_neighbours(G, V, [H | Rest], P, Acc) ->
    case (H == {V, V}) or (H == P) of
	true -> clean_neighbours(G, V, Rest, H, [H|Acc]);
	false -> clean_neighbours(G, V, Rest, H, Acc)
    end.    
	
remove_dirt(_G, _V, [])->
    ok;
remove_dirt(G, V, [H | Rest]) ->
    {N,_N} = H,
    remove_edge(G, V, N, [digraph:edge(G, E) || E <- digraph:edges(G,V)]),
    if 	
	V /= N -> 
	    remove_edge(G, N, V,
                            [digraph:edge(G, E) || E <- digraph:edges(G, V)]);
	true -> ok
    end,
    remove_dirt(G, V, Rest).
    
remove_edge(G, V, N, [H | Rest])->
    case H of 
	{Edge, V, N, _} ->
	    digraph:del_edge(G, Edge);
	_ -> remove_edge(G, V, N, Rest)
    end.

add_edge(G, {X, Y})->
    digraph:add_vertex(G, X, X),
    digraph:add_vertex(G, Y, Y),
    digraph:add_edge(G, X, Y),
    digraph:add_edge(G, Y, X).

add_edges(G, Edges)->
    [add_edge(G, Edge) || Edge <- Edges],
    G.

communities(G, MaxVertices) ->
    communities(G, digraph:no_vertices(G), MaxVertices).

communities(G, VerticesCount, MaxVertices) when VerticesCount > MaxVertices ->
    CommunitySizes = comm_sizes(G),
    % remove leaves before extracting communities
    % make a list of vertices to remove
    {Leaves, LeafCount}  = lists:foldl(
        fun(V, {L, D}) ->
            case digraph:in_degree(G, V) == 1 andalso digraph:out_degree(G, V) == 1 of
                true ->
                    [NV] = digraph:out_neighbours(G, V),
                    {[V|L], dict:update_counter(NV, 1, D)};
                _ ->
                    {L, D}
            end
        end, {[], dict:new()}, digraph:vertices(G)),
    % remove the leaves
    [digraph:del_vertex(G, V) || V <- Leaves],
    {EPs, IAs} = compute_communities(G),
    {EPs, IAs, LeafCount, CommunitySizes};
communities(G, _, _) ->
    CommunitySizes = comm_sizes(G),
    {EPs, IAs} = compute_communities(G),
    {EPs, IAs, dict:new(), CommunitySizes}.

compute_communities(G) ->
    % list of endpooints per community
    EPs = comm_endpoints(G),
    IAs = comm_interactions(G),
    {EPs, IAs}.

comm_sizes(G) ->
    lists:foldl(
        fun(V, D) ->
            {V, C} = digraph:vertex(G, V),
            dict:update_counter(C, 1, D)
        end, dict:new(), digraph:vertices(G)).

comm_endpoints(G) ->
    lists:foldl(
        fun(V, D) ->
            {V, C} = digraph:vertex(G, V),
            dict:append(C, V, D)
        end, dict:new(), digraph:vertices(G)).

comm_interactions(G) ->
    lists:foldl(
        fun(E, D) ->
            {E, V1, V2, _} = digraph:edge(G, E),
            {V1, C1} = digraph:vertex(G, V1),
            {V2, C2} = digraph:vertex(G, V2),
            case {C1, C2} of
                {C, C} ->
                    dict:append(C, {V1, V2}, D);
                {C1, C2} ->
                    D1 = dict:append(C1, {V1, V2}, D),
                    dict:append(C2, {V1, V2}, D1)
            end
        end, dict:new(), digraph:edges(G)).
