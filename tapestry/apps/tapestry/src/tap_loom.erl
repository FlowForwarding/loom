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

-behavior(gen_server).

-export([start_link/0]).

-export([ofsh_init/5,
         ofsh_connect/6,
         ofsh_disconnect/2,
         ofsh_failover/0,
         ofsh_handle_error/2,
         ofsh_handle_message/2,
         ofsh_terminate/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(STATE, tap_loom_state).
-record(?STATE,{
            config
            }).

-record(switch_config, {
    ip_addr,
    dpid,
    dns_port,
    client_port,
    dns_ips
}).

-include("tap_logger.hrl").
-include_lib("ofs_handler/include/ofs_handler.hrl").
-include_lib("of_protocol/include/of_protocol.hrl").

%------------------------------------------------------------------------------
% API Calls
%------------------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

ofsh_init(_Mode, IpAddr, DatapathId, Version, _Connection) ->
    ?INFO("Connection from ~p ~p~n", [IpAddr, DatapathId]),
    gen_server:cast(?MODULE, {initialize_switch, IpAddr, DatapathId, Version}).

ofsh_connect(_Mode, IpAddr, DatapathId, _Version, _Connection, AuxId) ->
    ?INFO("Connection from ~p ~p AuxId: ~p~n",
                                                [IpAddr, DatapathId, AuxId]),
    ok.

ofsh_disconnect(AuxId, DatapathId) ->
    ?INFO("Disconnect from ~p AuxId: ~p~n",
                                                        [DatapathId, AuxId]),
    ok.

ofsh_failover() ->
    % not implemented in ofs_handler
    ok.

ofsh_handle_error(DatapathId, Reason) ->
    ?INFO("Error from ~p Error: ~p~n", [DatapathId, Reason]),
    ok.

ofsh_handle_message(DatapathId, Msg) ->
    ?DEBUG("Message from ~p Message: ~p~n", [DatapathId, Msg]),
    ok.

ofsh_terminate(DatapathId) ->
    ?INFO("Terminate Main Connection from ~p~n", [DatapathId]),
    ok.

%------------------------------------------------------------------------------
% gen_server callbacks
%------------------------------------------------------------------------------

init([]) ->
    gen_server:cast(?MODULE, start),
    {ok, #?STATE{}}.

handle_call(Msg, From, State) ->
    error({no_handle_call, ?MODULE}, [Msg, From, State]).

handle_cast(start, State) ->
    Config = read_config(),
    {noreply, State#?STATE{config = Config}};
handle_cast({initialize_switch, IpAddr, DatapathId, Version},
                                        State = #?STATE{config = Config}) ->
    ofs_handler:subscribe(DatapathId, loom_handler, packet_in),
    install_flows(IpAddr, DatapathId, Version, Config),
    {noreply, State};
handle_cast(Msg, State) ->
    error({no_handle_cast, ?MODULE}, [Msg, State]).

handle_info(Msg, State) ->
    error({no_handle_info, ?MODULE}, [Msg, State]).

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

%------------------------------------------------------------------------------
% local functions
%------------------------------------------------------------------------------

read_config() ->
    Switches = tap_config:getallconfig(switch),
    lists:foldl(
        fun(Switch, ConfigDict) ->
            {Key, SwitchConfig} = switch_config(Switch),
            store_config(Key, SwitchConfig, ConfigDict)
        end, dict:new(), Switches).

store_config(Key, SwitchConfig, ConfigDict) ->
    case dict:is_key(Key, ConfigDict) of
        false ->
            dict:append(Key, SwitchConfig, dict:store(Key, [], ConfigDict));
        true ->
            dict:append(Key, SwitchConfig, ConfigDict)
    end.

switch_config(ConfigList) ->
    IpAddr = proplists:get_value(ip_addr, ConfigList),
    DatapathId = proplists:get_value(dpid, ConfigList),
    {switch_key(IpAddr, DatapathId),
     #switch_config{
        ip_addr = IpAddr,
        dpid = DatapathId,
        dns_port = proplists:get_value(dns_port, ConfigList),
        client_port = proplists:get_value(client_port, ConfigList),
        dns_ips = proplists:get_value(dns_ips, ConfigList)
     }}.

switch_key(undefined, undefined) ->
    error({badconfig, switch, no_ipaddr_or_datapathid});
switch_key(undefined, DatapathId) ->
    {dpid, DatapathId};
switch_key(IpAddr, undefined) ->
    {ipaddr, IpAddr};
switch_key(_, _) ->
    error({badconfig, switch, both_ipaddr_and_datapathid}).

install_flows(IpAddr, DatapathId, Version, Config) ->
    case get_switch_config(IpAddr, DatapathId, Config) of
        no_switch_config ->
            ?WARNING("No config for switch at ~p ~p~n", [IpAddr, DatapathId]),
            ok;
        SwitchConfigs ->
            % XXX remove flows
            [do_install_flows(SwitchConfig, DatapathId, Version) || SwitchConfig <- SwitchConfigs]
    end.

do_install_flows(#switch_config{
                    dns_port = Port1,
                    client_port = Port2,
                    dns_ips = IPs
                 }, DatapathId, Version) ->
    dns_tap([DatapathId], Version, Port1, Port2, IPs).

get_switch_config(IpAddr, DatapathId, Config) ->
    get_switch_config2([{dpid, DatapathId}, {ipaddr, IpAddr}], Config).

get_switch_config2([], _Config) ->
    no_switch_config;
get_switch_config2([Key | Rest], Config) ->
    case dict:find(Key, Config) of
        error ->
            get_switch_config2(Rest, Config);
        {ok, SwitchConfigs} ->
            SwitchConfigs
    end.


-define(L_PRIORITY, 100).
-define(H_PRIORITY, 101).


dns_tap([],_Version,_Port1,_Port2,_IPTupleList)->
    ok;
dns_tap(OFDPL, Version, Port1, Port2, IPTupleList)->
    [DatapathId|Rest] = OFDPL,
    IPList = [list_to_binary(tuple_to_list(IPTuple)) || IPTuple <- IPTupleList],
    ?DEBUG("dns_tap: ~p, ~p, ~p, ~p~n",
                                [DatapathId, Port1, Port2, IPTupleList]),
% XXX only remove all flows once per switch
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
forward_mod(Version, InPort, OutPorts)->
    Matches = [{in_port, <<InPort:32>>}],
    Instructions = [{apply_actions, [{output, OutPort, no_buffer} || OutPort <- OutPorts] } ],
    Opts = [{table_id,0}, {priority,?L_PRIORITY}, {idle_timeout, 0}, {idle_timeout, 0},
            {cookie, <<0,0,0,0,0,0,0,10>>}, {cookie_mask, <<0,0,0,0,0,0,0,0>>}],
    of_msg_lib:flow_add(Version, Matches, Instructions, Opts).    

% delete all flows in table 0
remove_all_flows_mod(Version) ->
    of_msg_lib:flow_delete(Version, [], [{table_id, 0}]).
