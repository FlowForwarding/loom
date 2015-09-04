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

-module(icontrol_logic).

-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(STATE, icontrol_logic_state).

-include("icontrol_logger.hrl").
-include_lib("ofs_handler/include/ofs_handler.hrl").
-include_lib("of_protocol/include/of_protocol.hrl").

-type switch_key() :: undefined | integer().

-record(?STATE, {
    next_switch_key = 1 :: integer(),
    switches_table :: term(), % ets table
    default_switch :: switch_key()
}).

-record(switch_record, {
    switch_key,
    ipaddr,
    datapath_id,
    version,
    connection
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
    sync_send/2,
    connect/2,
    close_connection/1,
    set_default/1,
    show_default/0,
    ofs_version/1]).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ----------------------------------------------------------------------------
%% Callback API
%% ----------------------------------------------------------------------------

% These functions are called from icontrol_ofsh.erl.
-spec ofsh_init(handler_mode(), ipaddress(), datapath_id(), of_version(), connection()) -> ok.
ofsh_init(active, IpAddr, DatapathId, Version, Connection) ->
    % new main connection
    ?INFO("new main connection: ~p ~p~n", [IpAddr, DatapathId]),
    ok = gen_server:call(?MODULE, {init, IpAddr, DatapathId, Version, Connection}),
    ok.

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

-spec ofsh_terminate(datapath_id()) -> ok.
ofsh_terminate(DatapathId) ->
    % lost the main connection
    ?INFO("disconnect main connection: ~p~n", [DatapathId]),
    ok = gen_server:call(?MODULE, {terminate, DatapathId}).

%% ----------------------------------------------------------------------------
%% Utility API
%% ----------------------------------------------------------------------------

%% @doc
%% Returns the list of connected switches.  The returned tuples have
%% the IP address of the switch (for calling icontrol_logic
%% functions), the datapath id (for calling ofs_handler),
%% the open flow version number (for calling of_msg_lib), the
%% connection (for calling of_driver).
%% @end
-spec switches() -> [{switch_key(), datapath_id(), ipaddress(), of_version(), connection()}].
switches() ->
    gen_server:call(?SERVER, switches).

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
-spec sync_send(switch_key() | default, ofp_message()) -> {ok, no_reply | ofp_message()} | {error, error_reason()}.
sync_send(SwitchKey, Msg) ->
    gen_server:call(?SERVER, {sync_send, SwitchKey, Msg}).

-spec connect(ipaddress(), inet:port_number()) -> ok | {error, error_reason()}.
connect(IpAddr, Port) ->
    case of_driver:connect(IpAddr, Port) of
        {ok, _} -> ok;
        Error -> Error
    end.

-spec close_connection(switch_key() | default) -> ok | {error, error_reason()}.
close_connection(SwitchKey) ->
    gen_server:call(?SERVER, {close_connection, SwitchKey}).

-spec set_default(switch_key()) -> ok | {error, error_reason()}.
set_default(SwitchKey) ->
    gen_server:call(?SERVER, {set_default, SwitchKey}).

-spec show_default() -> switch_key() | undefined.
show_default() ->
    gen_server:call(?SERVER, show_default).

-spec ofs_version(switch_key() | default) -> of_version() | {error, error_reason()}.
ofs_version(SwitchKey) ->
    gen_server:call(?SERVER, {ofs_version, SwitchKey}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    SwitchesTable = ets:new(switches, [{keypos, 2}, set, protected]),
    State = #?STATE{switches_table = SwitchesTable,
                    default_switch = undefined
                   },
    {ok, State}.

handle_call({init, IpAddr, DatapathId, Version, Connection}, _From, State) ->
    % Got the main connection, remember tha mapping between the ip address
    % and the datapath id
    NewState = register_switch(IpAddr, DatapathId, Version, Connection, State),
    {reply, ok, NewState};
handle_call({terminate, DatapathId}, _From, State) ->
    ok = deregister_switch(DatapathId, State),
    {reply, ok, State};
handle_call({sync_send, SwitchKey, Msg}, _From, State) ->
    Reply = do_sync_send(SwitchKey, Msg, State),
    {reply, Reply, State};
handle_call({close_connection, SwitchKey}, _From, State) ->
    Reply = do_close_connection(SwitchKey, State),
    {reply, Reply, State};
handle_call({set_default, SwitchKey}, _From, State) ->
    {Reply, NewState} = do_set_default(SwitchKey, State),
    {reply, Reply, NewState};
handle_call(show_default, _From,
                        State = #?STATE{default_switch = DefaultKey}) ->
    {reply, DefaultKey, State};
handle_call({ofs_version, SwitchKey}, _From, State) ->
    Reply = do_get_version(SwitchKey, State),
    {reply, Reply, State};
handle_call(switches, _From, State) ->
    Reply = [{SwitchKey, DatapathId, IpAddr, Version, Connection} ||
        #switch_record{
            switch_key = SwitchKey,
            datapath_id = DatapathId,
            ipaddr = IpAddr,
            version = Version,
            connection = Connection
        } <- do_get_switches(State)],
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

register_switch(IpAddr, DatapathId, Version, Connection,
                            State = #?STATE{switches_table = Switches,
                                            next_switch_key = SwitchKey}) ->
    SwitchRecord = #switch_record{
        switch_key = SwitchKey,
        ipaddr = IpAddr,
        datapath_id = DatapathId,
        version = Version,
        connection = Connection
    },
    true = ets:insert(Switches, SwitchRecord),
    NewState = maybe_set_switch_key(SwitchKey, State),
    NewState#?STATE{next_switch_key = SwitchKey + 1}.

maybe_set_switch_key(Key, State = #?STATE{default_switch = undefined}) ->
    State#?STATE{default_switch = Key};
maybe_set_switch_key(_, State) ->
    State.

deregister_switch(DatapathId, #?STATE{switches_table = Switches}) ->
    true = ets:match_delete(Switches,
                        #switch_record{datapath_id = DatapathId, _ = '_'}),
    ok.

find_switch(default, #?STATE{default_switch = undefined}) ->
    {error, no_default};
find_switch(default, State = #?STATE{default_switch = DefaultKey}) ->
    find_switch(DefaultKey, State);
find_switch(SwitchKey, #?STATE{switches_table = Switches}) ->
    case ets:lookup(Switches, SwitchKey) of
        [] -> {error, not_found};
        [SwitchRecord = #switch_record{}] -> SwitchRecord
    end.

do_get_switches(#?STATE{switches_table = Switches}) ->
    ets:tab2list(Switches).

do_get_version(Error = {error, _}, _State) ->
    Error;
do_get_version(#switch_record{version = Version}, _State) ->
    Version;
do_get_version(SwitchKey, State) ->
    do_get_version(find_switch(SwitchKey, State), State).

do_set_default(Error = {error, _}, State) ->
    {Error, State};
do_set_default(#switch_record{switch_key = DefaultKey}, State) ->
    {ok, State#?STATE{default_switch = DefaultKey}};
do_set_default(SwitchKey, State) ->
    do_set_default(find_switch(SwitchKey, State), State).

do_close_connection(Error = {error, _}, _State) ->
    Error;
do_close_connection(#switch_record{datapath_id = DatapathId}, _State) ->
    ofs_handler:terminate(DatapathId);
do_close_connection(SwitchKey, State) ->
    do_close_connection(find_switch(SwitchKey, State), State).

do_sync_send(Error = {error, _}, _Msg, _State) ->
    Error;
do_sync_send(#switch_record{datapath_id = DatapathId}, Msg, _State) ->
    ofs_handler:sync_send(DatapathId, Msg);
do_sync_send(SwitchKey, Msg, State) ->
    do_sync_send(find_switch(SwitchKey, State), Msg, State).
