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
%%% Stats Poller logic.
%%% @end

-module(stats_poller_logic).

-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(STATE, stats_poller_logic_state).

-include("stats_poller_logger.hrl").
-include_lib("ofs_handler/include/ofs_handler.hrl").
-include_lib("of_protocol/include/of_protocol.hrl").

-record(?STATE, {
    switches_table
}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

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
    ofsh_terminate/1,
    switches/0,
    send/2,
    sync_send/2,
    subscribe/2
]).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ----------------------------------------------------------------------------
%% Callback API
%% ----------------------------------------------------------------------------

% These functions are called from stats_poller_ofsh.erl.
-spec ofsh_init(handler_mode(), ipaddress(), datapath_id(), of_version(), connection()) -> ok.
ofsh_init(active, IpAddr, DatapathId, Version, Connection) ->
    % new main connection
    ?INFO("new active connection: ~p ~p~n", [IpAddr, DatapathId]),
    ok = gen_server:call(?MODULE, {init, IpAddr, DatapathId, Version, Connection}),
    ok;
ofsh_init(standby, IpAddr, DatapathId, _Version, _Connection) ->
    % new main connection
    % TODO: this will never happen, failover not implemented ofs_handler.
    ?INFO("new standby connection: ~p ~p~n", [IpAddr, DatapathId]),
    ok.

-spec ofsh_connect(handler_mode(), ipaddress(), datapath_id(), of_version(), connection(), auxid()) -> ok.
ofsh_connect(active, _IpAddr, DatapathId, _Version, _Connection, AuxId) ->
    % new auxiliary connection
    % The stats poller doesn't need to capture the auxiliary
    % connections, so they are not passed to the stats_poller_logic pid.
    ?INFO("new active aux connection: ~p ~p~n", [AuxId, DatapathId]),
    ok;
ofsh_connect(standby, _IpAddr, DatapathId, _Version, _Connection, AuxId) ->
    % new auxiliary connection
    % TODO: this will never happen, failover not implemented ofs_handler.
    ?INFO("new standby aux connection: ~p ~p~n", [AuxId, DatapathId]),
    ok.

-spec ofsh_disconnect(auxid(), datapath_id()) -> ok.
ofsh_disconnect(AuxId, DatapathId) ->
    % lost an auxiliary connection
    % The stats poller is not tracking the auxiliary
    % connections, so they are not passed to the stats_poller_logic pid.
    ?INFO("disconnect aux connection: ~p ~p~n", [AuxId, DatapathId]),
    ok.

-spec ofsh_failover() -> ok.
ofsh_failover() ->
    % ofs_handler failover
    % TODO: this will never happen, failover not implemented ofs_handler.
    ?INFO("failover"),
    ok.

-spec ofsh_handle_message(datapath_id(), ofp_message()) -> ok.
ofsh_handle_message(DatapathId, Msg) ->
    % process a message from the switch.
    % the stats poller doesn't process any messages
    % from the switch, so they are not passed to the stats_poller_logic pid.
    ?INFO("message in: ~p ~p~n", [DatapathId, Msg]),
    ok.

-spec ofsh_handle_error(datapath_id(), error_reason()) -> ok.
ofsh_handle_error(DatapathId, Reason) ->
    % Error on connection.
    ?INFO("rror in: ~p ~p~n", [DatapathId, Reason]),
    ok.

-spec ofsh_terminate(datapath_id()) -> ok.
ofsh_terminate(DatapathId) ->
    % lost the main connection
    ?INFO("disconnect main connection: ~p~n", [DatapathId]),
    ok = gen_server:call(?MODULE, {terminate, DatapathId}),
    ok.

%% ----------------------------------------------------------------------------
%% Utility API
%% ----------------------------------------------------------------------------

%% @doc
%% Returns the list of connected switches.  The returned tuples have
%% the IP address of the switch (for calling stats_poller_logic
%% functions), the datapath id (for calling ofs_handler),
%% the open flow version number (for calling of_msg_lib), the
%% connection (for calling of_driver), and the stats_poller_handler pid
%% responsible for polling for open flow stats from this switch.
%% @end
-spec switches() -> [{ipaddress(), datapath_id(), of_version(), connection(), pid()}].
switches() ->
    gen_server:call(?SERVER, switches).

%% @doc
%% Send ``Msg'' to the switch connected from ``IpAddr''.  Returns
%% ``not_found'' if there is no switch connected from ``IpAddrr'', ``ok''
%% if the message is sent successfully, or ``error'' if there was an error
%% sending the request to the switch.
%% @end
-spec send(ipaddress(), ofp_message()) -> not_found | ok | {error, error_reason()}.
send(IpAddr, Msg) ->
    gen_server:call(?SERVER, {send, IpAddr, Msg}).

%% @doc
%% Send ``Msg'' to the switch connected from ``IpAddr'' and wait
%% for any replies.  Returns
%% ``not_found'' if there is no switch connected from ``IpAddrr'',
%% ``{ok, Reply}''
%% if the message is sent successfully, or ``error'' if there was an error
%% sending the request to the switch.  ``Reply'' is ``no_reply'' if there
%% was no reply to the request, or ``Reply'' is an ``ofp_message'' record
%% that may be decoded with ``of_msg_lib:decode/1''.
%% @end
-spec sync_send(ipaddress(), ofp_message()) -> not_found | {ok, no_reply | ofp_message()} | {error, error_reason()}.
sync_send(IpAddr, Msg) ->
    gen_server:call(?SERVER, {sync_send, IpAddr, Msg}).

%% @doc
%% Subscribe to messages received from ``IpAddr''.
%% @end
-spec subscribe(ipaddress(), subscription_item()) -> ok.
subscribe(IpAddr, MsgType) ->
    gen_server:call(?SERVER, {subscribe, IpAddr, MsgType}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    SwitchesTable = ets:new(switches, [bag, protected]),
    State = #?STATE{switches_table = SwitchesTable},
    {ok, State}.

handle_call({init, IpAddr, DatapathId, Version, Connection}, _From, State) ->
    % Got the main connection, remember tha mapping between the ip address
    % and the datapath id
    {ok, Pid} = stats_poller_handler_sup:start_child(Version, DatapathId),
    ok = register_switch(IpAddr, DatapathId, Version, Connection, Pid, State),
    {reply, ok, State};
handle_call({terminate, DatapathId}, _From, State) ->
    Poller = poller_pid(DatapathId, State),
    ok = stats_poller_handler_sup:stop_child(Poller, normal),
    ok = deregister_switch(DatapathId, State),
    {reply, ok, State};
handle_call({sync_send, IpAddr, Msg}, _From, State) ->
    Reply = do_sync_send(IpAddr, Msg, State),
    {reply, Reply, State};
handle_call({send, IpAddr, Msg}, _From, State) ->
    Reply = do_send(IpAddr, Msg, State),
    {reply, Reply, State};
handle_call({subscribe, IpAddr, MsgType}, _From, State) ->
    ok = do_subscribe(IpAddr, MsgType, State),
    {reply, ok, State};
handle_call(switches, _From, State) ->
    Reply = do_get_switches(State),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

register_switch(IpAddr, DatapathId, Version, Connection, PollerPid, #?STATE{switches_table = Switches}) ->
    true = ets:insert(Switches, {IpAddr, DatapathId, Version, Connection, PollerPid}),
    ok.

deregister_switch(DatapathId, #?STATE{switches_table = Switches}) ->
    [Object] = ets:match_object(Switches, {'_', DatapathId, '_', '_', '_'}),
    true = ets:delete_object(Switches, Object),
    ok.

poller_pid(DatapathId, #?STATE{switches_table = Switches}) ->
    [[Pid]] = ets:match(Switches, {'_', DatapathId, '_', '_', '$1'}),
    Pid.

do_get_switches(#?STATE{switches_table = Switches}) ->
    ets:tab2list(Switches).

find_switch(IpAddr, #?STATE{switches_table = Switches}) ->
    % All LINC logical switches on the same capable switch connect
    % with the IP address of the capable switch.  There may be
    % duplicates.  The sne API doesn't really accomodate this, so
    % cheat by returning the first logical switch with the IP address.
    case ets:lookup(Switches, IpAddr) of
        [] -> not_found;
        [{_, DatapathId, _, _, _}|_] -> DatapathId
    end.

do_sync_send(IpAddr, Msg, State) ->
    case find_switch(IpAddr, State) of
        not_found -> not_found;
        DatapathId ->
            ofs_handler:sync_send(DatapathId, Msg)
    end.

do_send(IpAddr, Msg, State) ->
    case find_switch(IpAddr, State) of
        not_found -> not_found;
        DatapathId ->
            ofs_handler:send(DatapathId, Msg)
    end.

do_subscribe(IpAddr, MsgType, State) ->
    case find_switch(IpAddr, State) of
        not_found -> not_found;
        DatapathId ->
            % use our callback module to receive the handle_message.
            ofs_handler:subscribe(DatapathId, stats_poller_ofsh, MsgType)
    end.
