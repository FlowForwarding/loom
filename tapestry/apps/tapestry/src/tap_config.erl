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
         getenv/2,
         getconfig/1,
         getconfig/2,
         is_defined/2,
         refresh/0]).

-include("tap_logger.hrl").

getenv(Key) ->
    getenv(Key, undefined).

getenv(Key, Default) ->
    case application:get_env(tapestry, Key, Default) of
        undefined -> {error, not_found};
        Value -> Value
    end.

getconfig(Key) ->
    getenv(Key).

getconfig(Key, Default) ->
    getenv(Key, Default).

consult() ->
    ConfigFileName = getenv(config_file),
    file:consult(ConfigFileName).

is_defined(Element, Key) ->
    % XXX test is_defined!
    proplists:is_defined(Element, getconfig(Key)).

% populate the tapestry application environment with values from the
% tapestry.config file.  Called remotely by "tapestry config"
% to refresh the config
refresh() ->
    case consult() of
        {ok, Config} ->
            [application:set_env(tapestry, Key, Value) ||
                                        {Key, Value} <- flatten(Config)],
            ?INFO("tap_config: refreshed configuration values"),
            ok;
        {error, Reason} ->
            ?INFO("tap_config: config refresh error: ~p", [Reason]),
            {error, Reason}
    end.

% combine duplicate keys into a single key,value pair.  Value becomes
% a flattened list.
flatten(Config) ->
    lists:foldl(
        fun(K, L) ->
            [{K, flatten_value(proplists:get_all_values(K, Config))} | L]
        end, [], proplists:get_keys(Config)).

flatten_value([Value]) -> Value;
flatten_value(Values) -> Values.
