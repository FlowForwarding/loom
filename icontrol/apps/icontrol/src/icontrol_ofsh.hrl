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
%%% Simple network executive callback handler for ofs_handler.
%%% @end

% State held by ofs_handler.
% This state holds onto the datapath id and aux connection id.
% There is one state for each connection.  
-define(OFS_STATE, icontrol_ofs_state).
-record(?OFS_STATE, {
                    datapath_id,
                    aux_id = 0
                }).
-type ofs_state() :: #?OFS_STATE{}.
