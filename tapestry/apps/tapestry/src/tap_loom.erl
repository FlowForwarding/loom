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

-export([start_link/0,
         connect/2]).

-export([ofsh_init/5,
         ofsh_connect/6,
         ofsh_disconnect/2,
         ofsh_failover/0,
         ofsh_handle_error/2,
         ofsh_handle_message/2,
         ofsh_terminate/1]).

-export([packet_in/3]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(STATE, tap_loom_state).
-record(?STATE,{
            }).

-include("tap_logger.hrl").
-include_lib("ofs_handler/include/ofs_handler.hrl").
-include_lib("of_protocol/include/of_protocol.hrl").
-include_lib("pkt/include/pkt.hrl").

%------------------------------------------------------------------------------
% API Calls
%------------------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

connect(IpAddr, Port) ->
    gen_server:cast(?MODULE, {connect, IpAddr, Port}).

% @hidden
ofsh_init(_Mode, IpAddr, DatapathId, Version, _Connection) ->
    ?INFO("Connection from ~p ~p~n", [IpAddr, DatapathId]),
    tap_aggr:new_collector(DatapathId, IpAddr),
    gen_server:cast(?MODULE, {initialize_switch, IpAddr, DatapathId, Version}).

% @hidden
ofsh_connect(_Mode, IpAddr, DatapathId, _Version, _Connection, AuxId) ->
    ?INFO("Connection from ~p ~p AuxId: ~p~n", [IpAddr, DatapathId, AuxId]),
    ok.

% @hidden
ofsh_disconnect(AuxId, DatapathId) ->
    ?INFO("Disconnect from ~p AuxId: ~p~n", [DatapathId, AuxId]),
    ok.

% @hidden
ofsh_failover() ->
    % not implemented in ofs_handler
    ok.

% @hidden
ofsh_handle_error(DatapathId, Reason) ->
    ?INFO("Error from ~p Error: ~p~n", [DatapathId, Reason]),
    ok.

% @hidden
ofsh_handle_message(DatapathId, Msg) ->
    ?DEBUG("Message from ~p Message: ~p~n", [DatapathId, Msg]),
    ok.

% @hidden
ofsh_terminate(DatapathId) ->
    ?INFO("Terminate Main Connection from ~p~n", [DatapathId]),
    ok.

% @hidden
% Handling packet_in message
packet_in(DatapathId, IpAddr, Body) ->
    % received a message on the connection
    % this is processed by the callback pid, not this server
    Reason = proplists:get_value(reason, Body),
    TableId = proplists:get_value(table_id, Body),
    Match = proplists:get_value(match, Body),
    Data = proplists:get_value(data, Body),
    process_packetin(Reason, TableId, Match, Data, DatapathId, IpAddr),
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
    of_driver:listen(),
    Addrs = tap_config:getconfig(connect_to, []),
    connect_to_switches(Addrs),
    {noreply, State};
handle_cast({connect, IpAddr, Port}, State) ->
    connect_to_switch(IpAddr, Port),
    {noreply, State};
handle_cast({initialize_switch, _IpAddr, DatapathId, _Version}, State) ->
    ofs_handler:subscribe(DatapathId, tap_ofsh, packet_in),
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

connect_to_switches({error, _}) ->
    ok;
connect_to_switches(Addrs) ->
    ?DEBUG("connecting to ~p~n", [Addrs]),
    [connect(IpAddr, Port) || {IpAddr, Port} <- Addrs],
    ok.

connect_to_switch(IpAddr, Port) ->
    Response = of_driver:connect(IpAddr, Port),
    ?INFO("connecting to switch ~p ~p: ~p~n", [IpAddr, Port, Response]).

%packetin
process_packetin(action, _TableId, _Match, Data, DatapathId, IpAddr) ->
    dns_reply(Data, DatapathId, IpAddr);
process_packetin(nomatch, _TableId, _Match, _Data, _DatapathId, _IpAddr) ->
    ?DEBUG("packetin reason = nomatch~n");
process_packetin(Reason, _TableId, _Match, _Data, _DatapathId, _IpAddr) ->
    ?DEBUG("packetin reason = ~p~n", [Reason]).

dns_reply(Data, DatapathId, CollectorIP) ->
    % XXX apply white lists and black lists
    try
	Packet = pkt:decapsulate({ether, Data}),
	case Packet of 
	    [EthHeader, Header1, Header2, Payload] ->
		[ether|_] = tuple_to_list(EthHeader),
		[Type1|_] = tuple_to_list(Header1),
		[Type2|_] = tuple_to_list(Header2),
		Result = case (Type1 == ipv4) and (Type2 == udp) of
			     true ->  
				 inet_dns:decode(Payload);
			     _ -> unknown
			 end,
		case Result of
		    {ok, DnsRec} -> 
			Match = match_reply(DnsRec),
			case Match of
			    {error, _} ->
                                ?DEBUG("No match dropped: ~p, ~p~n",
				       [Match, DnsRec]);
			    {ok, ID, Query} ->
				R = list_to_tuple(
                                        binary_to_list(Header1#ipv4.daddr)),
				Interaction = {
                                   {R, [{who, requester}, 
                                        {label, tap_dns:gethostbyaddr(R)}]},
                                   {ID, [{who, resolved},
                                         {label, list_to_binary(Query)}]}},
				?DEBUG("Sending: ~p~n",[Interaction]),
                                tap_aggr:dns_reply(DatapathId, CollectorIP,
                                                                Interaction)
			end;
		    _ -> ?DEBUG("No match dropped: ~p~n",[Result])
		end;
	    _ -> ?DEBUG("No match dropped: ~p~n",[Packet])
	end
    catch
	Error ->
	    ?DEBUG("Decapsulation Error:~p Data: ~p~n",[Error, Data])
    end.

match_reply({dns_rec, {dns_header, _, true, _, _, _, _, _, _, _},
                      [{dns_query, Query, a, in}], RRSet, _, _})->
    Record = lists:keyfind(a, 3, RRSet),
    case Record of
	false ->
	    {error, no_a_record};
	{dns_rr, _, a, _, _, _, ID, _, _, _} -> {ok, ID, Query}
    end;
match_reply(_) ->
    {error, bad_response}.
