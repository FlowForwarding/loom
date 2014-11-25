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

-module (tap_batch_test).

-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------------------

tap_data_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
        {"parse_logfile", fun parse_logfile/0}
     ]
    }.

setup() ->
    ok.

cleanup(ok) ->
    ok.

%%------------------------------------------------------------------------------

parse_logfile() ->
    Bin = <<"
15-May-2014 13:33:18.468 client 192.168.11.172#50276: view 8: UDP: query: p14-keyvalueservice.icloud.com IN A response: NOERROR + p14-keyvalueservice.icloud.com. 86400 IN CNAME p14-keyvalueservice.icloud.com.akadns.net.; p14-keyvalueservice.icloud.com.akadns.net. 120 IN A 17.151.226.32; p14-keyvalueservice.icloud.com.akadns.net. 120 IN A 17.151.226.33;
15-May-2014 13:33:26.049 client 192.168.11.130#49974: view 8: UDP: query: www.isg-apple.com.akadns.net IN AAAA response: NOERROR + www.isg-apple.com.akadns.net. 27 IN CNAME www.apple.com.edgekey.net.; www.apple.com.edgekey.net. 465 IN CNAME e3191.dscc.akamaiedge.net.; e3191.dscc.akamaiedge.net.  20 IN AAAA 2001:418:142a:194::c77; e3191.dscc.akamaiedge.net.  20 IN AAAA 2001:418:142a:19d::c77; e3191.dscc.akamaiedge.net.  20 IN AAAA 2001:418:142a:18e::c77;
29-Oct-2014 09:48:02.588 client 2620:10a:6000:2000::2c6#7908: UDP: query: daisy.ubuntu.com IN A response: NOERROR + daisy.ubuntu.com.  339 IN A 91.189.92.55; daisy.ubuntu.com. 339 IN A 91.189.92.57;
29-Oct-2014 09:48:06.309 client 2620:10a:6000:2000::28c#23959: UDP: query: outlook.infoblox.com IN A response: NOERROR +A outlook.infoblox.com. 10 IN CNAME casarray1.infoblox.com.; casarray1.infoblox.com. 10 IN A 10.120.3.104;
">>,
    R = tap_batch:parse_logfile(Bin, fun(_,_) -> true end),
    ?assertMatch(
        [{<<"15-May-2014 13:33:18.468">>,{{192,168,11,172},_},{{17,151,226,32},_}},
         {<<"15-May-2014 13:33:26.049">>,{{192,168,11,130},_},{{8193,1048,5162,404,0,0,0,3191},_}},
         {<<"29-Oct-2014 09:48:02.588">>,{{9760,266,24576,8192,0,0,0,710},_},{{91,189,92,55},_}},
         {<<"29-Oct-2014 09:48:06.309">>,{{9760,266,24576,8192,0,0,0,652},_},{{10,120,3,104},_}}], R).
