-module(net_observer_logic).

-include("include/net_observer_logger.hrl").
-include_lib("ofs_handler/include/ofs_handler.hrl").
-include_lib("of_protocol/include/of_protocol.hrl").

-behaviour(gen_server).
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-define(STATE,net_observer_logic_state).
-record(?STATE, { graph_name
}).

-export([ ofsh_init/5,
          ofsh_connect/6,
          ofsh_disconnect/2,
          ofsh_failover/0,
          ofsh_handle_message/2,
          ofsh_handle_error/2,
          ofsh_terminate/1
]).
-export([ graphs/0,
          switches/0,
          switch/1
]).

%---------------------------------------------------------------------------------------------------------------
-spec ofsh_init(handler_mode(), ipaddress(), datapath_id(), of_version(), connection()) -> ok.
ofsh_init(active, IpAddr, DatapathId, Version, Connection) ->
    ?INFO("new main connection: ~p ~p~n", [IpAddr, DatapathId]),
    ok = gen_server:call(?MODULE, {init, IpAddr, DatapathId, Version, Connection}).
    %% Probably do some topology check here...

-spec ofsh_connect(handler_mode(), ipaddress(), datapath_id(), of_version(), connection(), auxid()) -> ok.
ofsh_connect(active, IpAddr, DatapathId, Version, Connection, AuxId) ->
    ?INFO("new aux connection: ~p ~p ~p~n", [IpAddr, AuxId, DatapathId]),
    ok = gen_server:call(?MODULE, {connect, IpAddr, DatapathId, Version, Connection, AuxId}).

-spec ofsh_disconnect(auxid(), datapath_id()) -> ok.
ofsh_disconnect(AuxId, DatapathId) ->
    ?INFO("disconnect aux connection: ~p ~p~n", [AuxId, DatapathId]),
    ok.

-spec ofsh_failover() -> ok.
ofsh_failover() ->
    ?INFO("failover"),
    ok.

-spec ofsh_handle_message(datapath_id(), ofp_message()) -> ok.
ofsh_handle_message(DatapathId, Msg) ->
    ?INFO("message in: ~p ~p~n", [DatapathId, Msg]),
    ok.

-spec ofsh_handle_error(datapath_id(), error_reason()) -> ok.
ofsh_handle_error(DatapathId, Reason) ->
    ?INFO("error in: ~p ~p~n", [DatapathId, Reason]),
    ok.

-spec ofsh_terminate(datapath_id()) -> ok.
ofsh_terminate(DatapathId) ->
    ?INFO("disconnect main connection: ~p~n", [DatapathId]),
    ok = gen_server:call(?MODULE, {terminate, DatapathId}).
%---------------------------------------------------------------------------------------------------------------
graphs() ->
    {ok,Gs} = dg:graphs(),
    [ GN || {GN,_DG} <- Gs].

switches() ->
    gen_server:call(?MODULE,switches).

switch(DPID) ->
    gen_server:call(?MODULE, {switch,DPID}).
%---------------------------------------------------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {}, []).

init({}) ->
    {ok,GN} = application:get_env(grapher,graph_name),
    {ok, #?STATE{graph_name=GN}}.

handle_call({init, IpAddr, DatapathId, Version, Connection}, _From, #?STATE{graph_name=GN} = State) ->
    ?INFO("Connection:~p\n",[Connection]),
    AV=dg:add_vertex(GN, list_to_binary(DatapathId),[{ip_address,inet:ntoa(IpAddr)},{version,Version},{aux_ids,[]}]),
    ?INFO("add_vertex ~p, ~p",[DatapathId,AV]),
    ok = discovery(),
    {reply, ok, State};
handle_call({connect, IpAddr, DatapathId, Version, Connection, AuxId}, _From, #?STATE{graph_name=GN} = State) ->
%% Aux Id startup order still breaks this ........
    case dg:vertex(GN, list_to_binary(DatapathId)) of 
        {ok,{_DatapathIdBin,Labels}} ->
            AuxIDList = proplists:get_value(aux_ids, Labels),
            case lists:member(AuxId,AuxIDList) of 
                true ->
                    ok; %% Why is it retrying the same aux id ?
                false ->
                    Labels2 = proplists:delete(aux_ids, Labels),
                    AV = dg:add_vertex(GN, list_to_binary(DatapathId), [{aux_ids,[AuxId|AuxIDList]}|Labels2]),
                    ?INFO("add_vertex ~p, ~p",[DatapathId,AV])
            end,
            {reply, ok, State};
        false ->
            ?INFO("Why would the DPID be missing ?, well the AUX ID startup in LINC is not ordered properly!"),
            {reply, ok, State}
    end;
handle_call({terminate, DatapathId}, _From, #?STATE{graph_name=GN} = State) ->
    DV=dg:del_vertex(GN, list_to_binary(DatapathId)),
    ?INFO("del_vertex ~p, ~p",[DatapathId,DV]),
    {reply, ok, State};
%---------------------------------------------------------------------------------------------------------------
handle_call(switches, _From, #?STATE{graph_name=GN} = State) ->
    GS=do_get_switches(GN),
    {reply, GS, State};
handle_call({switch,DPIDBin},_From,#?STATE{graph_name=GN} = State) ->
    Switch = dg:vertex(GN, DPIDBin),
    {reply,Switch,State};
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
%---------------------------------------------------------------------------------------------------------------
do_get_switches(GN) ->
    dg:vertices(GN).
    
discovery() ->
    ok.
    % Flow 


    % OutputToAll = #ofp_action_output{port = all},
    %                     PacketOut = Message#ofp_message{
    %                                   body = #ofp_packet_out{buffer_id = BufferId,
    %                                                          in_port = InPort,
    %                                                          actions = [OutputToAll],
    %                                                          data = Data}},