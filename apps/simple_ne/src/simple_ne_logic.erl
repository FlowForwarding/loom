-module(simple_ne_logic).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(STATE, simple_ne_logic_state).

-include_lib("loom/include/loom_logger.hrl").

-record(?STATE, {
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
    ofsh_init/4,
    ofsh_connect/5,
    ofsh_disconnect/3,
    ofsh_failover/1,
    ofsh_handle_packetin/3,
    ofsh_handle_message/3,
    ofsh_handle_error/3,
    ofsh_terminate/2
]).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ----------------------------------------------------------------------------
%% Callback API
%% ----------------------------------------------------------------------------

ofsh_init(active, IpAddr, DatapathId, Connection) ->
    ?INFO("new active connection: ~p ~p~n", [IpAddr, DatapathId]),
    {ok, self()};
ofsh_init(standby, IpAddr, DatapathId, Connection) ->
    ?INFO("new standby connection: ~p ~p~n", [IpAddr, DatapathId]),
    {ok, self()}.

ofsh_connect(active, IpAddr, DatapathId, Connection, AuxId) ->
    ?INFO("new active aux connection: ~p ~p~n", [AuxId, DatapathId]),
    {ok, self()};
ofsh_connect(standby, IpAddr, DatapathId, Connection, AuxId) ->
    ?INFO("new standby aux connection: ~p ~p~n", [AuxId, DatapathId]),
    {ok, self()}.

ofsh_disconnect(Pid, AuxId, DatapathId) ->
    ?INFO("disconnect aux connection: ~p ~p~n", [AuxId, DatapathId]),
    ok.

ofsh_failover(Pid) ->
    ?INFO("failover"),
    ok.

ofsh_handle_packetin(Pid, DatapathId, Packet) ->
    ?DEBUG("packetin: ~p ~p~n", [DatapathId, Packet]),
    ok.

ofsh_handle_message(Pid, DatapathId, Msg) ->
    ?DEBUG("message in: ~p ~p~n", [DatapathId, Msg]),
    ok.

ofsh_handle_error(Pid, DatapathId, Error) ->
    ?DEBUG("error in: ~p ~p~n", [DatapathId, Error]),
    ok.

ofsh_terminate(Pid, DatapathId) ->
    ?INFO("disconnect main connection: ~p~n", [DatapathId]),
    ok.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    State = #?STATE{},
    {ok, State}.

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

