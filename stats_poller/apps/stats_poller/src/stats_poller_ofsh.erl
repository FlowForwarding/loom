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
%%% Stats Poller callback handler for ofs_handler.
%%% @end

-module(stats_poller_ofsh).

-include("stats_poller_logger.hrl").
-include("stats_poller_ofsh.hrl").

-export([
    init/7,
    connect/8,
    disconnect/1,
    failover/1,
    handle_message/2,
    handle_error/2,
    terminate/1
]).

-type auxid() :: integer().
-type connection() :: pid().
-type datapath_id() :: term().
-type features() :: term().
-type handler_mode() :: active | standby.
-type ipaddress() :: {integer(), integer(), integer(), integer()}.
-type of_version() :: integer().
-type options() :: term().
-type error_reason() :: term().
-type ofp_message() :: term().

% callbacks from ofs_handler
% The callback functions in turn call stats_poller_logic for processing.
% TODO: handle standby connection
-spec init(handler_mode(), ipaddress(), datapath_id(), features(), of_version(), connection(), options()) -> {ok, ofs_state()}.
init(Mode, IpAddr, DatapathId, _Features, Version, _Connection, _Opts) ->
    % new main connection
    ?INFO("new main connection: ~p ~p ~p", [Mode, IpAddr, DatapathId]),
    {ok, Pid} = stats_poller_handler_sup:start_child(Version, DatapathId),
    State = #?OFS_STATE{datapath_id = DatapathId, handler = Pid},
    {ok, State}.

-spec connect(handler_mode(), ipaddress(), datapath_id(), features(), of_version(), connection(), auxid(), options()) -> {ok, ofs_state()}.
connect(Mode, IpAddr, DatapathId, _Features, _Version, _Connection, AuxId, _Opts) ->
    % new auxiliary connection
    ?INFO("new aux connection: ~p ~p ~p ~p",
                                        [Mode, IpAddr, AuxId, DatapathId]),
    State = #?OFS_STATE{datapath_id = DatapathId, aux_id = AuxId},
    {ok, State}.

-spec disconnect(ofs_state()) -> ok.
disconnect(State) ->
    % lost an auxiliary connection
    #?OFS_STATE{
        datapath_id = DatapathId,
        aux_id = AuxId
    } = State,
    ?INFO("disconnect aux connection: ~p ~p", [AuxId, DatapathId]),
    ok.

-spec failover(ofs_state()) -> ok.
failover(State) ->
    % State of new active
    % TODO: not failover not implement in ofs_handler
    {ok, State}.

-spec handle_error(error_reason(), ofs_state()) -> ok.
handle_error(Reason, State) ->
    % Error on the connection
    DatapathId = State#?OFS_STATE.datapath_id,
    ?INFO("error on connection: ~p ~p", [DatapathId, Reason]),
    {ok, State}.

-spec handle_message(ofp_message(), ofs_state()) -> ok.
handle_message(Msg, State) ->
    % received a message on the connection
    DatapathId = State#?OFS_STATE.datapath_id,
    ?INFO("message in: ~p ~p", [DatapathId, Msg]),
    {ok, State}.

-spec terminate(ofs_state()) -> ok.
terminate(#?OFS_STATE{handler = Handler,
                      datapath_id = DatapathId}) ->
    % lost the main connection
    ?INFO("lost main connection: ~p", [DatapathId]),
    ok = stats_poller_handler_sup:stop_child(Handler, normal).
