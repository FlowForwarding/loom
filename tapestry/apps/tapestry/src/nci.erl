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

-module(nci).

-export([compute_from_communities/1]).

-include("tap_logger.hrl").

%% === compute_from_communities ===
%% Compute the NCI from a list of communities
%%
%% Input: list where each element of the list of a tuple of two elements,
%%  the Community and the number of vertices in that community.
%%
%% Output: The Network Complexity Index for the input graph described in
%%         Bailey, Grossman. A Network Complexity Index for Networks of
%%         Networks. 2013
%%         http://www.flowforwarding.org/docs/Bailey%20-%20Grossman%20article%20on%20network%20complexity.pdf
%%
compute_from_communities(C) ->

    %% At this point in the code C refers to an unordered of community
    %% names with associated sizes
    %% For example if C = [{a,10},{f,3},{c,55}], it is a representation
    %% of three communities a, f, and c
    %% which have 10, 3, and 55 members respectively
    
    SC = ?LOGDURATION(rev_sort_labels(C)),

    %% At this point, SC referes to a sorted list of communities with
    %% sizes which have been sorted by size
    %% such that the largest communties are at the head of the list.
    %% Using the example C above,  SC = [{c,55},{a,10},{f,3}].

    %% Compute NCI from the sorted list of communities.
    ?LOGDURATION(calc_nci(SC, 0)).

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

%% Sort List on the size of the community
rev_sort_labels(List) ->
    lists:sort(fun({_, Size1},{_, Size2}) -> Size1 > Size2 end, List).
