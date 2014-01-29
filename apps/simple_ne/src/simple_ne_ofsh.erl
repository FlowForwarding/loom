-module(simple_ne_ofsh).

-export([
    init/7,
    connect/8,
    disconnect/1,
    failover/1,
    handle_message/2,
    handle_error/2,
    terminate/1
]).

-define(STATE, simple_ne_ofs_state).
-record(?STATE, {
                    datapath_id,
                    aux_id = 0,
                    handler_pid
}).

% callbacks from ofs_handler

init(Mode, IpAddr, DatapathId, _Features, _Version, Connection, _Opts) ->
    {ok, Pid} = simple_ne_logic:ofsh_init(Mode, IpAddr, DatapathId, Connection),
    State = #?STATE{
                        datapath_id = DatapathId,
                        handler_pid = Pid
                    },
    {ok, State}.

connect(Mode, IpAddr, DatapathId, _Features, _Version, Connection, AuxId, _Opts) ->
    {ok, Pid} = simple_ne_logic:ofsh_connect(Mode, IpAddr, DatapathId, Connection, AuxId),
    State = #?STATE{
                        datapath_id = DatapathId,
                        aux_id = AuxId,
                        handler_pid = Pid
                    },
    {ok, State}.

disconnect(State) ->
    #?STATE{
        datapath_id = DatapathId,
        aux_id = AuxId,
        handler_pid = Pid
    } = State,
    ok = simple_ne_logic:ofsh_disconnect(Pid, AuxId, DatapathId).

failover(State) ->
    % State of new active
    Pid = State#?STATE.handler_pid,
    ok = simple_ne_logic:ofsh_failover(Pid),
    {ok, State}.

handle_error(Reason, State) ->
    #?STATE{
        datapath_id = DatapathId,
        handler_pid = Pid
    } = State,
    ok = simple_ne_logic:ofsh_handle_error(Pid, DatapathId, Reason).

handle_message(Msg, State) ->
    #?STATE{
        datapath_id = DatapathId,
        handler_pid = Pid
    } = State,
    ok = simple_ne_logic:ofsh_handle_message(Pid, DatapathId, Msg).

terminate(State) ->
    #?STATE{
        datapath_id = DatapathId,
        handler_pid = Pid
    } = State,
    ok = simple_ne_logic:terminate(Pid, DatapathId).
