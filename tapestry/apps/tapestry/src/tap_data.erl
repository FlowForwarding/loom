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
%% @doc data utilities

-module(tap_data).

-export([sample_down/2]).

sample_down(List, MaxLen) when length(List) =< MaxLen ->
    List;
sample_down(List, MaxLen) ->
    sample_down1(List, ceiling(length(List)/MaxLen), []).

sample_down1([], _, Result) ->
    lists:reverse(Result);
sample_down1(List = [E | _], Interval, Result) ->
    sample_down1(nthtail(Interval, List), Interval, [E | Result]).

nthtail(Interval, List) when length(List) =< Interval ->
    [];
nthtail(Interval, List) ->
    lists:nthtail(Interval, List).

ceiling(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.

