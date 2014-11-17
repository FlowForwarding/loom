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
%% @copyright 2014 Infoblox Inc
%% @doc data utilities

-module(tap_dns).

-include_lib("kernel/include/inet.hrl").

-export([gethostbyaddr/1]).

gethostbyaddr(Addr) ->
    R = case inet:gethostbyaddr(Addr) of
        {ok, #hostent{h_name = Hostname}} ->
            Hostname;
        {error, Error} ->
            lists:flatten(io_lib:format("notfound_~p", [Error]))
    end,
    list_to_binary(R).
