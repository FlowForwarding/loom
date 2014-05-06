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

%%% @doc
%%% Simple network executive command line utility functions.
%%% @end

% controller connect
% set default switch (defaults to *first* switch connected)
% clear all flows
% forward
% bridge
% list flows
% of_msg_lib message
% function for version

% XXX disconnect not closing TCP connection

-module(iof).

-include_lib("ofs_handler/include/ofs_handler.hrl").
-include_lib("of_protocol/include/of_protocol.hrl").
-include("icontrol_logger.hrl").

-export([
    tr/0,
    version/0,
    version/1,
    debug/1,
    send/1,
    send/2,
    ping/0,
    ping/1,
    forward_mod/3,
    forward_mod/4,
    bridge/3,
    bridge/4,
    clear_flows0/0,
    clear_flows/1,
    clear_flows/2,
    flows/0,
    flows/1,
    dns_tap/5,
    dns_tap/6,
    tapestry_config/1,
    tapestry_config/2,
    tapestry_config/3,
    tapestry_config/4,
    connect/2,
    disconnect/0,
    disconnect/1,
    default/1,
    default/0,
    switches/0
]).

-type switch_key() :: integer().

%% @hidden
%% activate tracing for debugging
-spec tr() -> ok.
tr() ->
    dbg:start(),
    dbg:tracer(),
    dbg:p(all, c),
    dbg:tpl(ofs_handler_logic, []),
    dbg:tpl(icontrol_logic, []),
    dbg:tpl(icontrol_ofsh, []),
    ok.

%% @doc
%% Turn on or off debug messages to console log.
%% @end
-spec debug(on|off) -> ok.
debug(on) ->
    lager:set_loglevel(lager_console_backend, debug),
    ok;
debug(off) ->
    lager:set_loglevel(lager_console_backend, warning),
    ok.

%% @equiv send(default, Msg)
-spec send(Msg :: ofp_message()) -> {ok, ofp_message()} | {error, error_reason()}.
send(Msg = #ofp_message{}) ->
    send(default, Msg).

%% @doc
%% Send a message to the switch associated with Key and return the response.
%% If Key is ``default'', send message to the default switch.
%% @end
-spec send(Key :: switch_key(), Msg :: ofp_message()) -> {ok, ofp_message()} | {error, error_reason()}.
send(Key, Msg = #ofp_message{}) ->
    ?DEBUG("sending to ~p message ~p~n", [Key, Msg]),
    icontrol_logic:sync_send(Key, Msg).

%% @equiv ping(default)
ping() ->
    ping(default).

%% @doc
%% send an echo request to the switch associated with Key.
%% If Key is ``default'', send echo to the default switch.
%% @end
-spec ping(Key :: switch_key()) -> {ok, ofp_message()} | {error, error_reason()}.
ping(Key) ->
    Version = version(Key),
    Echo = of_msg_lib:echo_request(Version, <<>>),
    send(Key, Echo).


%% @equiv forward(default, Priority, InPort, OutPort)
forward_mod(Priority, InPort, OutPort) ->
    forward_mod(default, Priority, InPort, OutPort).

%% @doc
%% Forward all packets from InPort to OutPort on the switch associated
%% with Key.  Flows
%% are installed in table 0 with priority Priority.
%% If Key is ``default'', send forward mod to the default switch.
%% @end
-spec forward_mod(Key :: switch_key(), Priority :: integer(), InPort :: integer(), OutPort :: integer() | [integer()]) -> {ok, ofp_message()} | {error, error_reason()}.
forward_mod(Key, Priority, InPort, OutPorts) when is_list(OutPorts) ->
    Version = version(Key),
    Matches = [{in_port, <<InPort:32>>}],
    Instructions = [{apply_actions, [{output, OutPort, no_buffer} ||
                                                        OutPort <- OutPorts]}],
    Opts = [{table_id,0}, {priority, Priority},
            {idle_timeout, 0}, {idle_timeout, 0},
            {cookie, <<0,0,0,0,0,0,0,10>>},
            {cookie_mask, <<0,0,0,0,0,0,0,0>>}],
    Request = of_msg_lib:flow_add(Version, Matches, Instructions, Opts),
    send(Key, Request);
forward_mod(Key, Priority, InPort, OutPort) ->
    forward_mod(Key, Priority, InPort, [OutPort]).

%% @equiv bridge(default, Priority, Port1, Port2)
bridge(Priority, Port1, Port2) ->
    bridge(default, Priority, Port1, Port2).

%% @doc
%% Bridge ports Port1 and Port2 on the switch associated with Key.  Flows
%% are installed in table 0 with priority Priority.
%% If Key is ``default'', send forward mods to the default switch.
%% @end
-spec bridge(Key :: switch_key(), Priority :: integer(), Port1 :: integer(), Port2 :: integer()) -> [{ok, ofp_message()} | {error, error_reason()}].
bridge(Key, Priority, Port1, Port2) ->
    [forward_mod(Key, Priority, Port1, Port2),
     forward_mod(Key, Priority, Port2, Port1)].

%% @equiv clear_flows(default, 0)
-spec clear_flows0() -> {ok, ofp_message()} | {error, error_reason()}.
clear_flows0() ->
    clear_flows(default, 0).

%% @equiv clear_flows(default, TableId)
-spec clear_flows(TableId :: integer()) -> {ok, ofp_message()} | {error, error_reason()}.
clear_flows(TableId) ->
    clear_flows(default, TableId).

%% @doc
%% Clear all flows on the switch associated with Key.
%% If Key is ``default'', clear all flows on the default switch.
%% @end
-spec clear_flows(Key :: switch_key(), TableId :: integer()) -> {ok, ofp_message()} | {error, error_reason()}.
clear_flows(Key, TableId) ->
    Version = version(Key),
    Request = of_msg_lib:flow_delete(Version, [], [{table_id, TableId}]),
    send(Key, Request).

%% @equiv dns_tap(default, Priority, Port1, Port2, Port3, DnsIps)
dns_tap(Priority, Port1, Port2, Port3, DnsIps) ->
    dns_tap(default, Priority, Port1, Port2, Port3, DnsIps).

%% @doc
%% Forward udp packets from DnsIps on Port1 to Port2 and Port3.  Flows
%% are installed in table 0 with priority Priority.
%% If Key is ``default'', clear all flows on the default switch.
%% @end
-spec dns_tap(switch_key(), integer(), integer(), integer(), integer(), ipaddress() | [ipaddress()]) -> {ok, ofp_message()} | {error, error_reason()}.
dns_tap(Key, Priority, Port1, Port2, Port3, DnsIps) when is_list(DnsIps) ->
    [dns_tap(Key, Priority, Port1, Port2, Port3, DnsIp) || DnsIp <- DnsIps];
dns_tap(Key, Priority, Port1, Port2, Port3, DnsIp = {_,_,_,_}) ->
    Version = version(Key),
    IPv4Src = list_to_binary(tuple_to_list(DnsIp)),
    Matches = [{in_port, <<Port1:32>>},
               {eth_type, 2048},
               {ip_proto, <<17:8>>},
               {ipv4_src, IPv4Src}],
    Instructions = [{apply_actions, [{output, Port2, no_buffer},
                                     {output, Port3, no_buffer}]}],
    Opts = [{table_id,0}, {priority, Priority},
            {idle_timeout, 0}, {idle_timeout, 0},
            {cookie, <<0,0,0,0,0,0,0,10>>},
            {cookie_mask, <<0,0,0,0,0,0,0,0>>}],
    Msg = of_msg_lib:flow_add(Version, Matches, Instructions, Opts),
    send(Key, Msg).

%% @equiv tapestry_config(default, Port1, Port2, DnsIps)
-spec tapestry_config(integer(), integer(), [ipaddress()]) -> ok.
tapestry_config(Port1, Port2, DnsIps) ->
    tapestry_config(default, Port1, Port2, DnsIps).

%% @doc
%% Configure switch associated with Key for tapestry.  Remove all the
%% flows on table 0, dns_tap with priority 200 from Port1 to Port2 and
%% the controller.  Bridge between Port1 and Port2.
%% If Key is ``default'', clear all flows on the default switch.
%% @end
-spec tapestry_config(switch_key(), integer(), integer(), [ipaddress()]) -> ok.
tapestry_config(Key, Port1, Port2, DnsIps) ->
    ?DEBUG("clear_flows: ~p~n", [clear_flows(Key, 0)]),
    ?DEBUG("bridge: ~p~n", [bridge(Key, 100, Port1, Port2)]),
    ?DEBUG("dns_tap: ~p~n",
                [dns_tap(Key, 200, Port1, Port2, controller, DnsIps)]),
    ok.

%% @equiv tapestry_config(default, Filename)
-spec tapestry_config(string()) -> ok.
tapestry_config(Filename) ->
    tapestry_config(default, Filename).

%% @doc
%% If ``all'' is used as the Key, all connected switches described
%% in the tapestry config file are configured.  Otherwise, Key
%% identifies a particular switch to configure.  The Port and DNS server
%% IP addresses are read from the config file.  The configuration is
%% associated with the switch first by looking for matching datapath IDs
%% and next matching IP addresses.  The format of the tapestry
%% config file is:
%%
%% {switch, [{dpid, {0,<<8,0,39,197,149,72>>}},
%%           {dns_port, 1},
%%           {client_port, 2},
%%           {dns_ips, [{10,0,2,60}, {10,48,2,5}]}
%% ]}.
%% 
%% or
%% 
%% {switch, [{ip_addr, {192,168,56,102}},
%%           {dns_port, 1},
%%           {client_port, 2},
%%           {dns_ips, [{10,0,2,60}, {10,48,2,5}]}
%% ]}.
%% @end
-spec tapestry_config(all | switch_key(), string()) -> ok.
tapestry_config(all, Filename) ->
    Switches = icontrol_logic:switches(),
    Configs = consult_config(Filename),
    [try_config(Switch, Configs) || Switch <- Switches],
    ok;
tapestry_config(Key, Filename) when is_integer(Key); Key == default ->
    do_tapestry_config(Key, find_config(Key, consult_config(Filename))).

try_config({Key, DatapathId, IpAddr, _Version}, Configs) ->
    case do_find_config(DatapathId, IpAddr, Configs) of
        no_config ->
            ok;
        Config ->
            do_tapestry_config(Key, Config)
    end.

do_tapestry_config(Key, Config) ->
    ?DEBUG("configuring ~p with ~p~n", [Key, Config]),
    Port1 = config_value(dns_port, Config),
    Port2 = config_value(client_port, Config),
    DnsIps = config_value(dns_ips, Config),
    tapestry_config(Key, Port1, Port2, DnsIps).

consult_config(Filename) ->
    case file:consult(Filename) of
        {ok, Configs} -> [Body || {switch, Body} <- Configs];
        {error, Reason} -> error(Reason)
    end.

find_config(Key, Configs) ->
    {DatapathId, IpAddr} = find_switch(Key),
    case do_find_config(DatapathId, IpAddr, Configs) of
        no_config -> error(no_config);
        Config -> Config
    end.

do_find_config(DatapathId, IpAddr, Configs) ->
    FoundConfigs = [find_config(dpid, DatapathId, Configs) |
                        [find_config(ip_addr, IpAddr, Configs)]],
    case FoundConfigs of
        [no_config, no_config] -> no_config;
        [no_config, Config] -> Config;
        [Config1, _] -> Config1
    end.

find_config(Identifier, Identity, Configs) ->
    case lists:filter(
            fun(Config) ->
                case proplists:get_value(Identifier, Config) of
                    undefined -> false;
                    Identity -> true;
                    _ -> false
                end
            end, Configs) of
        [] -> no_config;
        [C | _] -> C
    end.

config_value(Key, Config) ->
    case proplists:get_value(Key, Config) of
        undefined -> error({Key, no_config});
        Value -> Value
    end.

find_switch(default) ->
    find_switch(default());
find_switch({error, Reason}) ->
    error(Reason);
find_switch(undefined) ->
    error(no_default);
find_switch(Key) when is_integer(Key) ->
    case lists:keyfind(Key, 1, icontrol_logic:switches()) of
        false -> error(no_config);
        {Key, DatapathId, IpAddr, _, _} -> {DatapathId, IpAddr}
    end.

%% @equiv flow(default)
-spec flows() -> term().
flows() ->
    flows(default).

%% @doc
%% Show flows on switch associated with Key.
%% If Key is ``default'', shows flows on the default switch.
%% @end
-spec flows(Key :: switch_key()) -> term().
flows(Key) ->
    Version = version(Key),
    Request = of_msg_lib:get_flow_statistics(Version, all, [], []),
    show(send(Key, Request)).

%% @doc
%% Force connection to Switch at IpAddr and Port
%% @doc
-spec connect(IpAddr :: ipaddress(), Port :: integer()) -> ok | {error, error_reason()}.
connect(IpAddr, Port) ->
    icontrol_logic:connect(IpAddr, Port).

%% @equiv disconnect(default)
disconnect() ->
    disconnect(default).

%% @doc
%% Disconnect from switch associated with Key.
%% If Key is ``default'', disconnect from the default switch.
%% @end
-spec disconnect(Key :: switch_key()) -> ok.
disconnect(Key) ->
    icontrol_logic:close_connection(Key).

%% @doc
%% Set the default switch key to Key.
%% @end
-spec default(Key :: switch_key()) -> ok | {error, error_reason()}.
default(Key) ->
    icontrol_logic:set_default(Key).

%% @doc
%% show default switch.
%% @end
-spec default() -> switch_key() | {error, error_reason()}.
default() ->
    icontrol_logic:show_default().

%% @doc
%% show switches
%% @end
-spec switches() -> ok.
switches() ->
    DefaultKey = default(),
    io:format(
"Switch-Key DatapathId                       IpAddr            Version~n"
"---------- -------------------------------- ----------------- -------~n"),
    [io:format("~1s~-9.10B ~-32w ~-17w ~-7.10B~n",
        [format_default(SwitchKey, DefaultKey), SwitchKey,
                                        DatapathId, IpAddr, Version]) ||
            {SwitchKey, DatapathId, IpAddr, Version, _} <-
                lists:sort(icontrol_logic:switches())],
    ok.

%% @equiv version(default)
version() ->
    version(default).

%% @doc
%% show OF protocol version of switch associated with Key.
%% @end
version(Key) ->
    check_version(icontrol_logic:ofs_version(Key)).

format_default(Key, Key) ->
    "*";
format_default(_, _) ->
    " ".

check_version({error, Reason}) ->
    error(Reason);
check_version(Version) ->
    Version.

show(List) when is_list(List) ->
    [show(E) || E <- List],
    ok;
show({error, Reason}) ->
    io:format("error: ~p~n", [Reason]);
show({ok, {ofp_message, _Version, _HdrType, _Xid, Body}}) ->
    io:format("~P~n", [Body, 10000]);
show(Msg) ->
    io:format("~P~n", [Msg, 10000]).
