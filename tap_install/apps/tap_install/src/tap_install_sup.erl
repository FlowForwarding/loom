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

-module(tap_install_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         start_child/2]).

%% Supervisor callbacks
-export([init/1]).

-include("tap_install_logger.hrl").

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    R = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
    ?INFO("config file name: ~s", [tap_install_config:configfile()]),
    make_connections(),
    R.

start_child(DatapathId, Version) ->
    supervisor:start_child(?MODULE, [DatapathId, Version]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {simple_one_for_one, 5, 10}, [
        ?CHILD(tap_install_logic, worker)
    ]} }.

%% ===================================================================
%% local
%% ===================================================================

make_connections() ->
    make_connection(tap_install_config:getallconfig(connect_to)).

make_connection([]) ->
    ok;
make_connection([{IpAddr, Port} | Rest]) ->
    ?INFO("connecting to: ~p:~p", [IpAddr, Port]),
    of_driver:connect(IpAddr, Port),
    make_connection(Rest).
