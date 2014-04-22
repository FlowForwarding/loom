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

-module(sne).

-include_lib("ofs_handler/include/ofs_handler.hrl").
-include_lib("of_protocol/include/of_protocol.hrl").

-export([
    tr/0,
    send/2,
    sync_send/2,
    subscribe/2,
    ping/1,
    i/1
]).

%% @hidden
-spec tr() -> ok.
tr() ->
    dbg:start(),
    dbg:tracer(),
    dbg:p(all, c),
    dbg:tpl(ofs_handler_logic, []),
    dbg:tpl(simple_ne_logic, []),
    dbg:tpl(simple_ne_ofsh, []),
    ok.

%% @doc
%% Send a message to the switch at ``IpAddr''.  Responses are returned
%% via the ``handle_message'' callback.  To see the messages, you must
%% create a subscription to the expected replies.
%% @end
-spec send(ipaddress(), ofp_message()) -> not_found | ok | {error, error_reason()}.
send(IpAddr, Msg) ->
    simple_ne_logic:send(IpAddr, Msg).

%% @doc
%% Send a message to the switch at ``IpAddr'' and wait for the reply.
%% @end
-spec sync_send(ipaddress(), ofp_message()) -> not_found | {ok, ofp_message()} | {error, error_reason()}.
sync_send(IpAddr, Msg) ->
    simple_ne_logic:sync_send(IpAddr, Msg).

%% @doc
%% Add a subscription.
%% @end
-spec subscribe(ipaddress(), subscription_item()) -> ok.
subscribe(IpAddr, MsgType) ->
    simple_ne_logic:subscribe(IpAddr, MsgType).

%% @doc
%% send an echo request to the server.  To see the response, first create
%% a subscription to the echo response: ``sne:subscribe(IpAddr, echo_reply)''.
%% @end
-spec ping(ipaddress()) -> not_found | ok | {error, error_reason}.
ping(IpAddr) ->
    Echo = of_msg_lib:echo_request(4, <<>>),
    send(IpAddr, Echo).

%% @doc
%% Forward packet from InPort to a OutPort or a list of OutPorts.
%% @end
%-spec forward_mod(ipaddress(), InPort :: integer(), OutPort :: integer() | OutPorts :: [integer()]) -> {ok, ofp_message()} | {error, error_reason()}.
%forward_mod(IpAddr, InPort, OutPort) when is_integer(OutPort) ->
    %forward_mod(IpAddr, InPort, [OutPort]);
%forward_mod(Version, InPort, OutPorts) when is_list(OutPorts) ->
    %Matches = [{in_port, <<InPort:32>>}],
    %Instructions = [{apply_actions, [{output, OutPort, no_buffer} || OutPort <- OutPorts]}],
    %Opts = [{table_id,0}, {priority, 100}, {idle_timeout, 0}, {idle_timeout, 0},
            %{cookie, <<0,0,0,0,0,0,0,10>>}, {cookie_mask, <<0,0,0,0,0,0,0,0>>}],
    %Request = of_msg_lib:flow_add(Version, Matches, Instructions, Opts),
    %simple_ne_logic:sync_send(IpAddr, Request).

%% @doc
%% Bridge port Port1 and Port2.
%% @end
%-spec bridge(ipaddress(), Port1 :: integer(), Port2 :: integer()) -> [{ok, ofp_message()} | {error, error_reason()}].
%bridge(IpAddr, Port1, Port2) ->
    %[forward_mod(IpAddr, Port1, Port2)), forward_mod(IpAddr, Port2, Port1))].
%
%% @doc
%% Clear all flows.
%% @end
%-spec clear_flows(ipaddress()) -> {ok, ofp_message()} | {error, error_reason()}.
%clear_flows(IpAddr) ->
    %Request = of_msg_lib:flow_delete(4, [], [{table_id, 0}]),
    %simple_ne_logic:sync_send(IpAddr, Request).

%% @doc
%% Get information about simple_ne state.
%%
%% ``switches'' - list the ip address, protocol version, and datapath id
%% of the connected switches.
%% @end
-spec i(Item:: atom()) -> term().
i(switches) ->
    lists:foreach(
        fun({IpAddr, DatapathId, Version, _}) ->
            io:format("~p: v~p ~p~n", [IpAddr, Version, DatapathId])
        end, simple_ne_logic:switches()).
