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
%%% Simple Network executive logic.
%%% @end

-module(tap_install_logic).

-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(STATE, tap_install_logic_state).

-include("tap_install_logger.hrl").
-include_lib("ofs_handler/include/ofs_handler.hrl").
-include_lib("of_protocol/include/of_protocol.hrl").

-record(?STATE, {
    datapath_id,
    version
}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-export([
    ofsh_init/5,
    ofsh_connect/6,
    ofsh_disconnect/2,
    ofsh_failover/0,
    ofsh_handle_message/2,
    ofsh_handle_error/2,
    ofsh_terminate/2
]).

start_link(DatapathId, Version) ->
    gen_server:start_link(?MODULE, [DatapathId, Version], []).

%% ----------------------------------------------------------------------------
%% Callback API
%% ----------------------------------------------------------------------------

% These functions are called from tap_install_ofsh.erl.
-spec ofsh_init(handler_mode(), ipaddress(), datapath_id(), of_version(), connection()) -> ok.
ofsh_init(active, IpAddr, DatapathId, Version, _Connection) ->
    % new main connection
    ?INFO("new main connection: ~p ~p v~p~n", [IpAddr, DatapathId, Version]),
    {ok, Pid} = tap_install_sup:start_child(DatapathId, Version),
    Pid.

-spec ofsh_connect(handler_mode(), ipaddress(), datapath_id(), of_version(), connection(), auxid()) -> ok.
ofsh_connect(active, IpAddr, DatapathId, _Version, _Connection, AuxId) ->
    % new auxiliary connection - ignored
    ?INFO("new aux connection: ~p ~p ~p~n", [IpAddr, AuxId, DatapathId]),
    ok.

-spec ofsh_disconnect(auxid(), datapath_id()) -> ok.
ofsh_disconnect(AuxId, DatapathId) ->
    % closed auxiliary connection - ignored
    ?INFO("disconnect aux connection: ~p ~p~n", [AuxId, DatapathId]),
    ok.

-spec ofsh_failover() -> ok.
ofsh_failover() ->
    % ofs_handler failover - not implemented, ignored
    ?INFO("failover"),
    ok.

-spec ofsh_handle_message(datapath_id(), ofp_message()) -> ok.
ofsh_handle_message(DatapathId, Msg) ->
    % process a message from the switch - print and ignore
    ?INFO("message in: ~p ~p~n", [DatapathId, Msg]),
    ok.

-spec ofsh_handle_error(datapath_id(), error_reason()) -> ok.
ofsh_handle_error(DatapathId, Reason) ->
    % Error on connection - print and ignore
    ?INFO("error in: ~p ~p~n", [DatapathId, Reason]),
    ok.

ofsh_terminate(DatapathId, Pid) ->
    % lost the main connection
    ?INFO("disconnect main connection: ~p~n", [DatapathId]),
    ok = gen_server:call(Pid, {terminate, DatapathId}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([DatapathId, Version]) ->
    State = #?STATE{
        datapath_id = DatapathId,
        version = Version
    },
    gen_server:cast(self(), install_flows),
    {ok, State}.

handle_call({terminate, DatapathId}, _From,
                            State = #?STATE{datapath_id = DatapathId}) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(install_flows, State = #?STATE{datapath_id = DatapathId,
                                           version = Version}) ->
    install_flows(DatapathId, Version),
    {noreply, State}.

handle_info(Info, State) ->
    error({no_handle_info, Info}),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% internal functions
%% ------------------------------------------------------------------

install_flows(DatapathId, Version) ->
    install_flows(DatapathId, Version, get_config(DatapathId)).

install_flows(DatapathId, _, no_config) ->
    ?INFO("install_flows(~p): no configured flows", [DatapathId]),
    ok;
install_flows(DatapathId, Version, {Port1, Port2, DnsIps}) ->
    tapestry_config_add(DatapathId, Version, Port1, Port2, DnsIps).

forward_mod(DatapathId, Version, Priority, InPort, OutPorts) when is_list(OutPorts) ->
    ?INFO("forward_mod(~p): Port ~p -> Ports ~p",
                                            [DatapathId, InPort, OutPorts]),
    Matches = [{in_port, <<InPort:32>>}],
    Instructions = [{apply_actions, [{output, OutPort, no_buffer} ||
                                                        OutPort <- OutPorts]}],
    Opts = [{table_id,0}, {priority, Priority},
            {idle_timeout, 0}, {idle_timeout, 0},
            {cookie, <<0,0,0,0,0,0,0,10>>},
            {cookie_mask, <<0,0,0,0,0,0,0,0>>}],
    Request = of_msg_lib:flow_add(Version, Matches, Instructions, Opts),
    send(DatapathId, Request);
forward_mod(DatapathId, Version, Priority, InPort, OutPort) ->
    forward_mod(DatapathId, Version, Priority, InPort, [OutPort]).

bridge(DatapathId, Version, Priority, Port1, Port2) ->
    [forward_mod(DatapathId, Version, Priority, Port1, Port2),
     forward_mod(DatapathId, Version, Priority, Port2, Port1)].

dns_tap(DatapathId, Version, Priority, Port1, Port2, Port3, DnsIps) when is_list(DnsIps) ->
    [dns_tap(DatapathId, Version, Priority, Port1, Port2, Port3, DnsIp) || DnsIp <- DnsIps];
dns_tap(DatapathId, Version, Priority, Port1, Port2, Port3, DnsIp = {_,_,_,_}) ->
    ?INFO("dns_tap(~p): Port ~p -> Port ~p, Port ~p, DNS IP: ~p",
                                    [DatapathId, Port1, Port2, Port3, DnsIp]),
    IPv4Src = list_to_binary(tuple_to_list(DnsIp)),
    % Matches must be in a specific order, otherwise of_msg_lib will
    % complain about missing or bad required fields.
    Matches = [{in_port, <<Port1:32>>},
               {eth_type, <<8,0>>},
               {ip_proto, <<17:8>>},
               {udp_src, <<53:16>>},
               {ipv4_src, IPv4Src}],
    Instructions = [{apply_actions, [{output, Port2, no_buffer},
                                     {output, Port3, no_buffer}]}],
    Opts = [{table_id,0}, {priority, Priority},
            {idle_timeout, 0}, {idle_timeout, 0},
            {cookie, <<0,0,0,0,0,0,0,10>>},
            {cookie_mask, <<0,0,0,0,0,0,0,0>>}],
    Msg = of_msg_lib:flow_add(Version, Matches, Instructions, Opts),
    send(DatapathId, Msg).

tapestry_config_add(DatapathId, Version, Port1, Port2, DnsIps) ->
    BridgeResponse = bridge(DatapathId, Version, 100, Port1, Port2),
    ?INFO("bridge(~p): ~p~n", [DatapathId, BridgeResponse]),
    TapResponse = dns_tap(DatapathId, Version, 200,
                                        Port1, Port2, controller, DnsIps),
    ?INFO("dns_tap(~p): ~p~n", [DatapathId, TapResponse]),
    ok.

send(DatapathId, Msg = #ofp_message{}) ->
    ?DEBUG("send(~p): ~p", [DatapathId, Msg]),
    ofs_handler:sync_send(DatapathId, Msg).

get_config(DatapathId) ->
    DefaultConfig = config(default),
    DatapathConfig = config(DatapathId),
    case {DefaultConfig, DatapathConfig} of
        {undefined, undefined} ->
            no_config;
        {_, undefined} ->
            DefaultConfig;
        {_, _} ->
            DatapathConfig
    end.

config(Key) ->
    case tap_install_config:getconfig(Key) of
        {error, not_found} ->
            undefined;
        PL ->
            [Port1, Port2, DnsIps] =
                [proplists:get_value(K, PL) || K <- [port1, port2, dns_ips]],
            {Port1, Port2, DnsIps}
    end.
