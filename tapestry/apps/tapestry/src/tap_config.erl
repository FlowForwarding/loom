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
%% @doc tap module for parsing the tapestry.config file

-module(tap_config).

-export([getenv/1, getconfig/1]).

getenv(Key) ->
    case application:get_env(tapestry, Key) of
        undefined -> {error, not_found};
        {ok, Value} -> {Key, Value}
    end.

getconfig(Key) ->
    {config_file, ConfigFileName} = getenv(config_file),
    ConfigFile = file:consult(ConfigFileName),
    case ConfigFile of
	{ok, Config}->
	    {Key, proplists:get_value(Key, Config, env_value(Key))};
	{error, Reason} ->
            {error, no_config, Reason}
    end.

env_value(Key) ->
    case application:get_env(tapestry, Key) of
        undefined -> {error, not_found};
        {ok, Value} -> Value
    end.
