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

-module (tap_data_test).

-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------------------

tap_data_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
        {"sample_down", fun sample_down/0}
     ]
    }.

setup() ->
    ok.

cleanup(ok) ->
    ok.

%%------------------------------------------------------------------------------

sample_down() ->
    ?assertEqual([1,2,3], tap_data:sample_down([1,2,3], 4)),
    ?assertEqual([1,2,3], tap_data:sample_down([1,2,3], 3)),
    ?assertEqual([1,3], tap_data:sample_down([1,2,3,4], 2)),
    ?assertEqual([1,5], tap_data:sample_down([1,2,3,4,5,6,7], 2)),
    ?assertEqual([1,5], tap_data:sample_down([1,2,3,4,5,6,7,8], 2)),
    ?assertEqual([1,6], tap_data:sample_down([1,2,3,4,5,6,7,8,9], 2)).
