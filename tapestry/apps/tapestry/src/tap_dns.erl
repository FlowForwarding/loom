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

-export([gethostbyaddr/1,
         allow/3,
         mkmask/2,
         intaddr/1,
         binaryaddr/1]).

gethostbyaddr(Addr) ->
    R = case inet:gethostbyaddr(Addr) of
        {ok, #hostent{h_name = Hostname}} ->
            Hostname;
        {error, Error} ->
            lists:flatten(io_lib:format("notfound_~p", [Error]))
    end,
    list_to_binary(R).

allow(IpAddr, WhiteList, BlackList) ->
    IpAddrI = intaddr(binaryaddr(IpAddr)),
    mask(IpAddrI, WhiteList) andalso (not mask(IpAddrI, BlackList)).

intaddr(<<I:32>>) ->
    {32, I};
intaddr(<<I:128>>) ->
    {128, I}.

binaryaddr({A,B,C,D}) ->
    <<A:8,B:8,C:8,D:8>>;
binaryaddr({A,B,C,D,E,F,G,H}) ->
    <<A:16,B:16,C:16,D:16,E:16,F:16,G:16,H:16>>.

mkmask(Addr = {_,_,_,_}, Length) ->
    <<M:Length,_/bits>> = <<16#ff:8,16#ff:8,16#ff:8,16#ff:8>>,
    Mask = <<M:Length,0:(32-Length)>>,
    {intaddr(Mask), intaddr(binaryaddr(Addr))};
mkmask(Addr = {_,_,_,_,_,_,_,_}, Length) ->
    <<M:Length,_/bits>> = <<16#ffff:16,16#ffff:16,16#ffff:16,16#ffff:16,
                              16#ffff:16,16#ffff:16,16#ffff:16,16#ffff:16>>,
    Mask = <<M:Length,0:(128-Length)>>,
    {intaddr(Mask), intaddr(binaryaddr(Addr))}.

% true if mask in MaskList allows IpAddrB
mask(_, []) ->
    false;
mask(Addr = {Size, IpAddr}, [{{Size, Mask}, {Size, Value}} | Rest]) ->
    case (IpAddr band Mask) == Value of
        true -> true;
        false ->
            mask(Addr, Rest)
    end;
mask(Addr, [_ | Rest]) ->
    mask(Addr, Rest).
