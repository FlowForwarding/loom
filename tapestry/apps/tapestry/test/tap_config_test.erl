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

-module (tap_config_test).

-include_lib("eunit/include/eunit.hrl").

-define(ENVKEY, eunit).
-define(UNKNOWN_ENVKEY, not_a_key).
-define(ENVVALUE, testvalue).
-define(CONFIGFILE, "../test/tapestry_test.config").
-define(UNKNOWN_CONFIGFILE, "not_a_config_file").
-define(CONFIGKEY, config_eunit).
-define(UNKNOWN_CONFIGKEY, not_a_config_key).
-define(CONFIGVALUE, config_testvalue).

%%------------------------------------------------------------------------------

tap_config_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
        {"getenv", fun getenv/0}
       ,{"getenv no key", fun getenv_nokey/0}
       ,{"getconfig", fun getconfig/0}
       ,{"getconfig no file", fun getconfig_nofile/0}
       ,{"getconfig no key", fun getconfig_nokey/0}
       ,{"getconfig no env key", fun getconfig_noenv/0}
     ]
    }.

setup() ->
    application:load(tapestry),
    application:set_env(tapestry, ?ENVKEY, ?ENVVALUE),
    ok.

cleanup(ok) ->
    application:unload(tapestry),
    ok.

%%------------------------------------------------------------------------------

getenv() ->
    ?assertEqual({?ENVKEY, ?ENVVALUE}, tap_config:getenv(?ENVKEY)).

getenv_nokey() ->
    ?assertEqual({error, not_found}, tap_config:getenv(?UNKNOWN_ENVKEY)).

getconfig() ->
    application:set_env(tapestry, config_file, ?CONFIGFILE),
    ?assertEqual({?CONFIGKEY, ?CONFIGVALUE}, tap_config:getconfig(?CONFIGKEY)).

getconfig_nofile() ->
    application:set_env(tapestry, config_file, ?UNKNOWN_CONFIGFILE),
    ?assertEqual({error, no_config, enoent},
                                    tap_config:getconfig(?UNKNOWN_CONFIGKEY)).

getconfig_nokey() ->
    application:set_env(tapestry, config_file, ?CONFIGFILE),
    ?assertEqual({?ENVKEY, ?ENVVALUE}, tap_config:getconfig(?ENVKEY)).

getconfig_noenv() ->
    application:set_env(tapestry, config_file, ?CONFIGFILE),
    ?assertEqual({?UNKNOWN_ENVKEY, {error, not_found}},
                                        tap_config:getconfig(?UNKNOWN_ENVKEY)).
