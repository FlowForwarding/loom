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
%% @copyright 2013 Infoblox Inc
%% @doc Network tap

-module(tap_yaws).

-compile([export_all]).

-include("../include/tapestry.hrl").

start()->
    start(?TAP_DEFAULT_HOST,?YAWS_DEFAULT_ADDRESS,?TAP_DEFAULT_PORT).

start(Servername,IPAddress,Port) ->
    start(Servername,IPAddress,Port,[]).
    
start(_Servername,IPAddress,Port,ConfOptions) when is_list(ConfOptions) ->
    crypto:start(),
    {ok,ParsedAddress} = inet_parse:ipv4_address(IPAddress),
    code:add_pathz(?YAWS_EBIN_DIR),
    code:add_pathz(?JIFFY_EBIN_DIR),
    file:make_dir(?YAWS_LOG_DIR),
    error_logger:info_msg("Starting Embedded Yaws!~n"),
    GL = [
	  {logdir,?YAWS_LOG_DIR},
	  {ebin_dir, [?YAWS_EBIN_DIR]}],
    SL = [
	  {port,Port},
	  {listen,ParsedAddress}], 
    yaws:start_embedded(?YAWS_DOC_ROOT,SL,GL),
    self().

restart()->
    yaws:stop(),
    start().
    
