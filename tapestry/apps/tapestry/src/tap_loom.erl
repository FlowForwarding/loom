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

%------------------------------------------------------------------------------
% gen_server callbacks
%------------------------------------------------------------------------------

init([]) ->
    gen_server:cast(?MODULE, start),
    {ok, #?STATE{}}.

handle_call(Msg, From, State) ->
    error({no_handle_call, ?MODULE}, [Msg, From, State]).

handle_cast(start, State) ->
    Addrs = tap_config:getallconfig(connect_to),
    connect_to_switches(Addrs),
    {noreply, State};
handle_cast({connect, IpAddr, Port}, State) ->
    connect_to_switch(IpAddr, Port),
    {noreply, State};
handle_cast({initialize_switch, _IpAddr, DatapathId, _Version}, State) ->
    ofs_handler:subscribe(DatapathId, loom_handler, packet_in),
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
