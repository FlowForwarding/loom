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
%% @doc time utilities

-module(tap_time).

-export([now/0,
         since/1,
         diff/2,
         diff_millis/2,
         universal/1,
         universal_now/0,
         universal_time_diff/2,
         rfc3339/1,
         rfc3339_to_epoch/1]).

now() ->
    os:timestamp().

since(A) ->
    diff(tap_time:now(), A).

% time difference (B - A) in seconds
diff(A, B) ->
    diff_millis(A, B) div 1000.

diff_millis(A, B) ->
    timer:now_diff(A, B) div 1000.

universal(T) ->
    calendar:now_to_universal_time(T).

universal_now() ->
    universal(tap_time:now()).

universal_time_diff(A, B) ->
    calendar:datetime_to_gregorian_seconds(B) -
                                calendar:datetime_to_gregorian_seconds(A).

rfc3339({{Year, Month, Day}, {Hour, Minute, Second}})->
    lists:flatten(
        io_lib:format("~4.4.0w-~2.2.0w-~2.2.0wT~2.2.0w:~2.2.0w:~2.2.0wZ",
            [Year, Month, Day, Hour, Minute, Second])).

rfc3339_to_epoch(Timestamp)->
    {ok, [Year,Month,Day,Hour,Minute,Second],[]} =
                        io_lib:fread("~4d-~2d-~2dT~2d:~2d:~2dZ", Timestamp),
    {{Year,Month,Day},{Hour, Minute, Second}}.
