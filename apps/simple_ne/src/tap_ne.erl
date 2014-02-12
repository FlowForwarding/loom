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
%% @doc tap module for loom

-module(tap_ne).
-copyright("2013, Infoblox Inc.").

-include_lib("ofs_handler/include/ofs_handler.hrl").
-include_lib("of_protocol/include/of_protocol.hrl").
-include_lib("of_protocol/include/ofp_v4.hrl").

-export([config/0]).

-define(Version, 4).
-define(L_PRIORITY, 100).
-define(H_PRIORITY, 101).

config() ->
    [{IP, _, _, _, _}] = simple_ne_logic:switches(),
    sne:send(IP, remove_all_flows_mod()),
    sne:send(IP, tap_dns_response(1,2,controller, IP)),
    sne:send(IP, forward_mod(1, [2])),
    sne:send(IP, forward_mod(2, [1])).

%tap packets to controller for udp traffic from DNS server IP address
tap_dns_response(Port1, Port2, Port3, IPv4Src) ->
%    Matches = [{in_port, <<Port1:32>>}, {eth_type, <<16#800:16>>}, {ip_proto, <<17:8>>}, {ipv4_src, IPv4Src}],
    Matches = [{in_port, <<Port1:32>>}, {eth_type, 2048}, {ip_proto, <<17:8>>}, {ipv4_src, IPv4Src} ],
    Instructions = [{apply_actions, [{output, Port2, no_buffer}, {output, Port3, no_buffer}] }],
    Opts = [{table_id,0}, {priority, ?H_PRIORITY}, {idle_timeout, 0}, {idle_timeout, 0},
            {cookie, <<0,0,0,0,0,0,0,10>>}, {cookie_mask, <<0,0,0,0,0,0,0,0>>}],
    of_msg_lib:flow_add(?Version, Matches, Instructions, Opts).

%forward packet on InPort to OutPorts using apply_actions
forward_mod(InPort,OutPorts)->
    Matches = [{in_port, <<InPort:32>>}],
    Instructions = [{apply_actions, [{output, OutPort, no_buffer} || OutPort <- OutPorts] } ],
    io:format("Instructions=~p~n", [Instructions]),
    Opts = [{table_id,0}, {priority,?L_PRIORITY}, {idle_timeout, 0}, {idle_timeout, 0},
            {cookie, <<0,0,0,0,0,0,0,10>>}, {cookie_mask, <<0,0,0,0,0,0,0,0>>}],
    of_msg_lib:flow_add(?Version, Matches, Instructions, Opts).    

% delete all flows in table 0
remove_all_flows_mod() ->
    of_msg_lib:flow_delete(?Version, [], [{table_id, 0}]).
    
