-module(simple_ne_logic).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(STATE, simple_ne_logic_state).

-include_lib("loom/include/loom_logger.hrl").

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
    ofsh_disconnect/3,
    ofsh_failover/1,
    ofsh_handle_message/3,
    ofsh_handle_error/3,
    ofsh_terminate/2,
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

ofsh_init(active, IpAddr, DatapathId, Version, Connection) ->
    ok = gen_server:call(?MODULE, {init, IpAddr, DatapathId, Version, Connection}),
    ?INFO("new active connection: ~p ~p~n", [IpAddr, DatapathId]),
    {ok, self()};
ofsh_init(standby, IpAddr, DatapathId, _Version, _Connection) ->
    ?INFO("new standby connection: ~p ~p~n", [IpAddr, DatapathId]),
    {ok, self()}.

ofsh_connect(active, _IpAddr, DatapathId, _Connection, _Version, AuxId) ->
    ?INFO("new active aux connection: ~p ~p~n", [AuxId, DatapathId]),
    {ok, self()};
ofsh_connect(standby, _IpAddr, DatapathId, _Connection, _Version, AuxId) ->
    ?INFO("new standby aux connection: ~p ~p~n", [AuxId, DatapathId]),
    {ok, self()}.

ofsh_disconnect(_Pid, AuxId, DatapathId) ->
    ?INFO("disconnect aux connection: ~p ~p~n", [AuxId, DatapathId]),
    ok.

ofsh_failover(_Pid) ->
    ?INFO("failover"),
    ok.

ofsh_handle_message(_Pid, DatapathId, Msg) ->
    ?INFO("message in: ~p ~p~n", [DatapathId, Msg]),
    ok.

ofsh_handle_error(_Pid, DatapathId, Reason) ->
    ?INFO("rror in: ~p ~p~n", [DatapathId, Reason]),
    ok.

ofsh_terminate(_Pid, DatapathId) ->
    ?INFO("disconnect main connection: ~p~n", [DatapathId]),
    ok.

%% ----------------------------------------------------------------------------
%% Utility API
%% ----------------------------------------------------------------------------

switches() ->
    gen_server:call(?SERVER, switches).

send(IpAddr, Msg) ->
    gen_server:call(?SERVER, {send, IpAddr, Msg}).

sync_send(IpAddr, Msg) ->
    gen_server:call(?SERVER, {sync_send, IpAddr, Msg}).

subscribe(IpAddr, MsgType) ->
    gen_server:call(?SERVER, {subscribe, IpAddr, MsgType}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    SwitchesTable = ets:new(switches, [set, protected]),
    State = #?STATE{switches_table = SwitchesTable},
    {ok, State}.

handle_call({init, IpAddr, DatapathId, Version, Connection}, _From, State) ->
    ok = register_switch(IpAddr, DatapathId, Version, Connection, State),
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

register_switch(IpAddr, DatapathId, Version, Connection, #?STATE{switches_table = Switches}) ->
    true = ets:insert(Switches, {IpAddr, DatapathId, Version, Connection}),
    ok.

do_get_switches(#?STATE{switches_table = Switches}) ->
    ets:tab2list(Switches).

find_switch(IpAddr, #?STATE{switches_table = Switches}) ->
    case ets:lookup(Switches, IpAddr) of
        [] -> not_found;
        [{_, DatapathId, _, _}] -> DatapathId
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
            ofs_handler:subscribe(DatapathId, simple_ne_ofsh, MsgType)
    end.
