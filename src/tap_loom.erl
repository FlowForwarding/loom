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

-compile([export_all]).

start()->
    [code:add_pathz(Path) || Path <- filelib:wildcard("./lib/loom/ebin")],
    [code:add_pathz(Path) || Path <- filelib:wildcard("./lib/loom/deps/*/ebin")],
    [code:add_pathz(Path) || Path <- filelib:wildcard("./lib/loom/apps/*/ebin")],
    application:start(mnesia),
    application:start(syntax_tools),
    application:start(compiler),
    ok = application:start(lager),
    ok = application:start(eenum),
    ok = application:start(of_protocol),
    application:start(of_msg_lib),
    ok = application:start(of_driver),
    application:start(ofs_handler),
    application:start(loom).
    

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

process_config([],IP)->
    ok;
process_config([Config|Rest],IP) ->
    case Config of
	{ofdps,OFDPSConfig}->
	    process_ofdps(OFDPSConfig,IP);
	_ -> process_config(Rest,IP)
    end.


process_ofdps([],TargetIP)->
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
					  dns_tap([DataPathId],Port1,Port2,IPs);
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
	    OFDPList = loom_ofdp:get_all(default),
	    lists:foreach(fun(X)->
				  {OFDPIP,_} = loom_ofdp:get_address(X),
				  case OFDPIP == IPAddr of
				      true ->
					  dns_tap([X],Port1,Port2,IPs);
				      false -> ok
				  end
			  end, OFDPList),
	    process_ofdps(Rest);
	_ -> process_ofdps(Rest)
    end.

    


dns_tap([],_Port1,_Port2,_IPTupleList)->
    ok;
dns_tap(OFDPL,Port1,Port2,IPTupleList)->
    [DatapathId|Rest] = OFDPL,
    IPList = [ list_to_binary(tuple_to_list(IPTuple)) || IPTuple <- IPTupleList ],
    error_logger:info_msg("dns_tap: ~p, ~p, ~p, ~p~n",[OFDP,Port1,Port2,IPTupleList]),
    ofs_handler:send(DatapathId, remove_all_flows_mod(Version)),
    lists:foreach(fun(X)->ofs_handler:send(DatapathId, tap_dns_response(Version, Port1,Port2,controller,X)) end,IPList),
    ofs_handler:send(DatapathId, forward_mod(Version, Port1, [Port2])),
    ofs_handler:send(DatapathId, forward_mod(Version, Port2, [Port1]))
    dns_tap(Rest,Port1,Port2,IPTupleList).
