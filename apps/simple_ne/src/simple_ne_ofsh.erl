%%%-------------------------------------------------------------------
%%% @copyright (C) 1999-2013, Erlang Solutions Ltd
%%% @author Marc Sugiyama <marc.sugiyama@erlang-solutions.com>
%%% @doc
%%% Simple network executive callback handler for ofs_handler.
%%% @end
%%%-------------------------------------------------------------------
-module(simple_ne_ofsh).
-copyright("2013, Erlang Solutions Ltd.").

-include_lib("ofs_handler/include/ofs_handler.hrl").
-include_lib("of_protocol/include/of_protocol.hrl").
-include("simple_ne_ofsh.hrl").

-export([
    init/7,
    connect/8,
    disconnect/1,
    failover/1,
    handle_message/2,
    handle_error/2,
    terminate/1
]).

% callbacks from ofs_handler
% The callback functions in turn call simple_ne_logic for processing.
-spec init(handler_mode(), ipaddress(), datapath_id(), features(), of_version(), connection(), options()) -> {ok, ofs_state()}.
init(Mode, IpAddr, DatapathId, _Features, Version, Connection, _Opts) ->
    % new main connection
    ok = simple_ne_logic:ofsh_init(Mode, IpAddr, DatapathId, Version, Connection),
    State = #?OFS_STATE{datapath_id = DatapathId},
    {ok, State}.

-spec connect(handler_mode(), ipaddress(), datapath_id(), features(), of_version(), connection(), auxid(), options()) -> {ok, ofs_state()}.
connect(Mode, IpAddr, DatapathId, _Features, Version, Connection, AuxId, _Opts) ->
    % new auxiliary connection
    ok = simple_ne_logic:ofsh_connect(Mode, IpAddr, DatapathId, Version, Connection, AuxId),
    State = #?OFS_STATE{datapath_id = DatapathId, aux_id = AuxId},
    {ok, State}.

-spec disconnect(ofs_state()) -> ok.
disconnect(State) ->
    % lost an auxiliary connection
    #?OFS_STATE{
        datapath_id = DatapathId,
        aux_id = AuxId
    } = State,
    ok = simple_ne_logic:ofsh_disconnect(AuxId, DatapathId).

-spec failover(ofs_state()) -> ok.
failover(State) ->
    % State of new active
    % TODO: not failover not implement in ofs_handler
    ok = simple_ne_logic:ofsh_failover(),
    {ok, State}.

-spec handle_error(error_reason(), ofs_state()) -> ok.
handle_error(Reason, State) ->
    % Error on the connection
    DatapathId = State#?OFS_STATE.datapath_id,
    ok = simple_ne_logic:ofsh_handle_error(DatapathId, Reason).

-spec handle_message(ofp_message(), ofs_state()) -> ok.
handle_message(Msg, State) ->
    % received a message on the connection
    DatapathId = State#?OFS_STATE.datapath_id,
    ok = simple_ne_logic:ofsh_handle_message(DatapathId, Msg).

-spec terminate(ofs_state()) -> ok.
terminate(State) ->
    % lost the main connection
    DatapathId = State#?OFS_STATE.datapath_id,
    ok = simple_ne_logic:ofsh_terminate(DatapathId).
