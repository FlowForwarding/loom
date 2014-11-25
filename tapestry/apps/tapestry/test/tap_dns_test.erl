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

-module (tap_dns_test).

-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------------------

tap_data_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         {"binaryaddr", fun binaryaddr/0}
        ,{"intaddr", fun intaddr/0}
        ,{"mkmask", fun mkmask/0}
        ,{"white list", fun white_list/0}
        ,{"black list", fun black_list/0}
        ,{"combo", fun combo/0}
        ,{"none", fun none/0}
        ,{"re", fun re/0}
     ]
    }.

setup() ->
    ok.

cleanup(ok) ->
    ok.

%%------------------------------------------------------------------------------

binaryaddr() ->
    ?assertEqual(<<1,2,3,4>>, tap_dns:binaryaddr({1,2,3,4})),
    ?assertEqual(<<1:16,2:16,3:16,4:16,5:16,6:16,7:16,8:16>>,
                                    tap_dns:binaryaddr({1,2,3,4,5,6,7,8})).

intaddr() ->
    ?assertEqual({32, 1 *256*256*256 +
                      2 *256*256 +
                      3 *256 +
                      4}, tap_dns:intaddr(tap_dns:binaryaddr({1,2,3,4}))),
    ?assertEqual({128, 1 *65536*65536*65536*65536*65536*65536*65536 +
                       2 *65536*65536*65536*65536*65536*65536 +
                       3 *65536*65536*65536*65536*65536 +
                       4 *65536*65536*65536*65536 +
                       5 *65536*65536*65536 +
                       6 *65536*65536 +
                       7 *65536 +
                       8},
                    tap_dns:intaddr(tap_dns:binaryaddr({1,2,3,4,5,6,7,8}))).

mkmask() ->
    ?assertEqual({{32, 255 *256*256*256}, {32, 10 *256*256*256}},
                                            tap_dns:mkmask({10,0,0,0}, 8)),
    ?assertEqual({{32, 255 *256*256*256 +
                       240  *256*256},
                  {32, 172 *256*256*256 +
                        16 *256*256}},
                                            tap_dns:mkmask({172,16,0,0}, 12)),
    ?assertEqual({{128, 254*256 *65536*65536*65536*65536*65536*65536*65536},
                  {128, 252     *65536*65536*65536*65536*65536*65536*65536}},
                                    tap_dns:mkmask({252,0,0,0,0,0,0,0}, 7)).

white_list() ->
    % allow all private networks
    WhiteList = [tap_dns:mkmask({10,0,0,0},8),
                 tap_dns:mkmask({172,16,0,0},12),
                 tap_dns:mkmask({192,168,0,0},16),
                 tap_dns:mkmask({
                     16#fc00,0,0,0,0,0,0,0}, 7)],
    BlackList = [],
    ?assertNot(tap_dns:allow({1,2,3,4}, WhiteList, BlackList)),
    ?assertNot(tap_dns:allow({1,2,3,4,5,6,7,8}, WhiteList, BlackList)),
    ?assert(tap_dns:allow({10,1,2,3}, WhiteList, BlackList)),
    ?assert(tap_dns:allow({172,16,2,3}, WhiteList, BlackList)),
    ?assert(tap_dns:allow({172,17,2,3}, WhiteList, BlackList)),
    ?assert(tap_dns:allow({192,168,2,3}, WhiteList, BlackList)),
    ?assert(tap_dns:allow({252 *256,2,3,4,5,6,7,8}, WhiteList, BlackList)),
    ok.

black_list() ->
    % don't allow all private networks
    WhiteList = [tap_dns:mkmask({0,0,0,0},0),
                 tap_dns:mkmask({0,0,0,0,0,0,0,0},0)],
    BlackList = [tap_dns:mkmask({10,0,0,0},8),
                 tap_dns:mkmask({172,16,0,0},12),
                 tap_dns:mkmask({192,168,0,0},16),
                 tap_dns:mkmask({
                     16#fc00,0,0,0,0,0,0,0}, 7)],
    ?assert(tap_dns:allow({1,2,3,4}, WhiteList, BlackList)),
    ?assert(tap_dns:allow({1,2,3,4,5,6,7,8}, WhiteList, BlackList)),
    ?assertNot(tap_dns:allow({10,1,2,3}, WhiteList, BlackList)),
    ?assertNot(tap_dns:allow({172,16,2,3}, WhiteList, BlackList)),
    ?assertNot(tap_dns:allow({172,17,2,3}, WhiteList, BlackList)),
    ?assertNot(tap_dns:allow({192,168,2,3}, WhiteList, BlackList)),
    ?assertNot(tap_dns:allow({252 *256,2,3,4,5,6,7,8}, WhiteList, BlackList)),
    ok.

combo() ->
    % allow 10.0.0.0 private networks
    WhiteList = [tap_dns:mkmask({10,0,0,0},8)],
    % do not allow 10.11.12.13
    BlackList = [tap_dns:mkmask({10,11,12,13},32)],
    ?assertNot(tap_dns:allow({1,2,3,4}, WhiteList, BlackList)),
    ?assertNot(tap_dns:allow({1,2,3,4,5,6,7,8}, WhiteList, BlackList)),
    ?assert(tap_dns:allow({10,1,2,3}, WhiteList, BlackList)),
    ?assertNot(tap_dns:allow({10,11,12,13}, WhiteList, BlackList)),
    ok.

none() ->
    WhiteList = [tap_dns:mkmask({0,0,0,0},0),
                 tap_dns:mkmask({0,0,0,0,0,0,0,0},0)],
    BlackList = [],
    ?assert(tap_dns:allow({1,2,3,4}, WhiteList, BlackList)),
    ?assert(tap_dns:allow({1,2,3,4,5,6,7,8}, WhiteList, BlackList)).

re() ->
    WhiteList = [tap_dns:mkre(RE) || RE <- [".com$", ".org$"]],
    BlackList = [tap_dns:mkre(RE) || RE <- ["google.com$", "acm.org$"]],
    ?assert(tap_dns:allowquery("yahoo.com", WhiteList, BlackList)),
    ?assert(tap_dns:allowquery("hoopla.org", WhiteList, BlackList)),
    ?assert(tap_dns:allowquery("oogle.com", WhiteList, BlackList)),
    ?assertNot(tap_dns:allowquery("google.com", WhiteList, BlackList)),
    ?assertNot(tap_dns:allowquery("mail.google.com", WhiteList, BlackList)),
    ?assertNot(tap_dns:allowquery("acm.org", WhiteList, BlackList)),
    ?assertNot(tap_dns:allowquery("pickle.acm.org", WhiteList, BlackList)).

%-------------------------------------------------------------------------------
% helpers
%-------------------------------------------------------------------------------

trace() ->
    dbg:start(),
    dbg:tracer(),
    dbg:p(all, c),
    dbg:tpl(tap_dns, mkmask, [{'_',[],[{return_trace}]}]),
    dbg:tpl(tap_dns, mask, [{'_',[],[{return_trace}]}]).

