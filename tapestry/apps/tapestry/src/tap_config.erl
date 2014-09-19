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

-export([getenv/1,
         getconfig/1,
         getallconfig/1,
         is_defined/2]).

getenv(Key) ->
    case application:get_env(tapestry, Key) of
        undefined -> {error, not_found};
        {ok, Value} -> Value
    end.

% return config value if present
% return env value if config not present
% return config value even if there is no env value
% return error if no config or env value
getconfig(Key) ->
    case consult() of
        {ok, Config} ->
            case {proplists:get_value(Key, Config),
                                        application:get_env(tapestry, Key)} of
                {undefined, undefined} ->
                    {error, not_found};
                {undefined, {ok, Default}} ->
                    Default;
                {Value, _} ->
                    Value
            end;
        {error, Reason} ->
            {error, Reason}
    end.

getallconfig(Key) ->
    case consult() of
	{ok, Config}->
	    proplists:get_all_values(Key, Config);
	{error, Reason} ->
            {error, Reason}
    end.

consult() ->
    ConfigFileName = getenv(config_file),
    file:consult(ConfigFileName).

is_defined(Element, Key) ->
    proplists:is_defined(Element, getconfig(Key)).
