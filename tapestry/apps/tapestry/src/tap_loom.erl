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
%% @copyright 2013,2014 Infoblox Inc
%% @doc tap module for loom

-module(tap_loom).

-include_lib("ofs_handler/include/ofs_handler.hrl").
-include_lib("of_protocol/include/of_protocol.hrl").
-include_lib("of_protocol/include/ofp_v4.hrl").

-compile([export_all]).

-define(L_PRIORITY, 100).
-define(H_PRIORITY, 101).

config()->
    ConfigFile = file:consult("tapestry.config"),
    case ConfigFile of
	{ok,Config}->
	    process_config(Config);
	_ -> {error,no_config}
    end.

config(IP)->
    ConfigFile = file:consult("tapestry.config"),
    case ConfigFile of
	{ok,Config}->
	    process_config(Config,IP);
	_ -> {error,no_config}
    end.


process_config([])->
    ok;
process_config([Config|Rest]) ->
    case Config of
	{ofdps,OFDPSConfig}->
	    process_ofdps(OFDPSConfig);
	_ -> process_config(Rest)
    end.

process_config([],_IP)->
    ok;
process_config([Config|Rest],IP) ->
    case Config of
	{ofdps,OFDPSConfig}->
	    process_ofdps(OFDPSConfig,IP);
	_ -> process_config(Rest,IP)
    end.


process_ofdps([],_TargetIP)->
        ok;
process_ofdps([OFDP|Rest],TargetIP) ->
    case OFDP of
	{ofdp,{ip_addr,IPAddr},DNSPort,ClientPort,DNSIps} when (IPAddr == TargetIP)->
	    error_logger:info_msg("processing: ~p~n",[OFDP]),
	    {dns_port,Port1} = DNSPort,
	    {client_port,Port2} = ClientPort,
	    {dns_ips,IPs} = DNSIps,
	    OFSwitchList = simple_ne_logic:switches(),
	    lists:foreach(fun(X)->
				  {OFDPIP, DatapathId, Version, _, _} = X,
				  case OFDPIP == IPAddr of
				      true ->
					  dns_tap([DatapathId],Version,Port1,Port2,IPs);
				      false -> ok
				  end
			  end, OFSwitchList),
	    process_ofdps(Rest,TargetIP);
	_ -> process_ofdps(Rest,TargetIP)
    end.



process_ofdps([])->
    ok;
process_ofdps([OFDP|Rest]) ->
    io:format("process_ofdps: ofdp = ~p, Rest = ~p~n",[OFDP,Rest]),
    case OFDP of
	{ofdp,IP,DNSPort,ClientPort,DNSIps}->
	    error_logger:info_msg("processing: ~p~n",[OFDP]),
	    {ip_addr,IPAddr} = IP,
	    {dns_port,Port1} = DNSPort,
	    {client_port,Port2} = ClientPort,
	    {dns_ips,IPs} = DNSIps,
	    OFSwitchList = simple_ne_logic:switches(),
	    lists:foreach(fun(X)->
				  {OFDPIP, DatapathId, Version, _, _} = X,
				  case OFDPIP == IPAddr of
				      true ->
					  dns_tap([DatapathId],Version,Port1,Port2,IPs);
				      false -> ok
				  end
			  end, OFSwitchList),
	    process_ofdps(Rest);
	_ -> process_ofdps(Rest)
    end.

bridge(IPAddress,Port1,Port2) ->
    OFSwitchList = simple_ne_logic:switches(),
    lists:foreach(fun(X)->
			  {OFDPIP, DatapathId, Version, _, _} = X,
			  case OFDPIP == IPAddress of
			      true ->
				  ofs_handler:send(DatapathId, forward_mod(Version, Port1, [Port2])),
				  ofs_handler:send(DatapathId, forward_mod(Version, Port2, [Port1]));
			      false -> ok
			  end
		  end, OFSwitchList).

clear_flows(IPAddress) -> 
    OFSwitchList = simple_ne_logic:switches(),
    lists:foreach(fun(X)->
			  {OFDPIP, DatapathId, Version, _, _} = X,
			  case OFDPIP == IPAddress of
			      true ->
				  ofs_handler:send(DatapathId, remove_all_flows_mod(Version));
			      false -> ok
			  end
		  end, OFSwitchList).



dns_tap([],_Version,_Port1,_Port2,_IPTupleList)->
    ok;
dns_tap(OFDPL,Version,Port1,Port2,IPTupleList)->
    [DatapathId|Rest] = OFDPL,
    IPList = [ list_to_binary(tuple_to_list(IPTuple)) || IPTuple <- IPTupleList ],
    error_logger:info_msg("dns_tap: ~p, ~p, ~p, ~p~n",[DatapathId,Port1,Port2,IPTupleList]),
    ofs_handler:send(DatapathId, remove_all_flows_mod(Version)),
    lists:foreach(fun(X)->ofs_handler:send(DatapathId, tap_dns_response(Version, Port1,Port2,controller,X)) end,IPList),
    ofs_handler:send(DatapathId, forward_mod(Version, Port1, [Port2])),
    ofs_handler:send(DatapathId, forward_mod(Version, Port2, [Port1])),
    dns_tap(Rest,Version,Port1,Port2,IPTupleList).


%tap packets to controller for udp traffic from DNS server IP address
tap_dns_response(Version, Port1, Port2, Port3, IPv4Src) ->
%     Matches = [{in_port, <<Port1:32>>}, {eth_type, 2048}, {ip_proto, <<17:8>>}],
    Matches = [{in_port, <<Port1:32>>}, {eth_type, 2048}, {ip_proto, <<17:8>>}, {ipv4_src, IPv4Src} ],
    Instructions = [{apply_actions, [{output, Port2, no_buffer}, {output, Port3, no_buffer}] }],
    Opts = [{table_id,0}, {priority, ?H_PRIORITY}, {idle_timeout, 0}, {idle_timeout, 0},
            {cookie, <<0,0,0,0,0,0,0,10>>}, {cookie_mask, <<0,0,0,0,0,0,0,0>>}],
    of_msg_lib:flow_add(Version, Matches, Instructions, Opts).

%forward packet on InPort to OutPorts using apply_actions
forward_mod(Version,InPort,OutPorts)->
    Matches = [{in_port, <<InPort:32>>}],
    Instructions = [{apply_actions, [{output, OutPort, no_buffer} || OutPort <- OutPorts] } ],
    io:format("Instructions=~p~n", [Instructions]),
    Opts = [{table_id,0}, {priority,?L_PRIORITY}, {idle_timeout, 0}, {idle_timeout, 0},
            {cookie, <<0,0,0,0,0,0,0,10>>}, {cookie_mask, <<0,0,0,0,0,0,0,0>>}],
    of_msg_lib:flow_add(Version, Matches, Instructions, Opts).    

% delete all flows in table 0
remove_all_flows_mod(Version) ->
    of_msg_lib:flow_delete(Version, [], [{table_id, 0}]).


    
%%% Handling packet_in message
%%-spec handle_message(ofp_message(), ofs_state()) -> ok.
handle_message({packet_in, _Xid, Body}, _State) ->
    % received a message on the connection
    Data = proplists:get_value(data, Body),
    io:format("Data = ~p~n", [Data]),
    ok.
