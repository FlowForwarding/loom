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

-compile([export_all]).


get(Param)->
    ConfigFile = file:consult("tapestry.config"),
    case ConfigFile of
	{ok,Config}->
	    get_value(Config,Param,{errork,not_found});
	_ -> {error,no_config}
    end.

get_value([],Param,_Result)->
    {error,not_found};
get_value(_Config,[],Result)->
    Result;
get_value([CH|CRest],[PH|PRest],Result) ->
    {Type,Values} = CH,
    case Type == PH of
	true -> get_value(Values,PRest,CH);
	false -> get_value(CRest,[PH|PRest],Result)
    end.
