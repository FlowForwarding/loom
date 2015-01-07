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
         allowquery/3,
         mkre/1,
         intaddr/1,
         binaryaddr/1,
         inet_parse_address/1,
         new_cache/0,
         cache_get/2]).

-define(MAX_CACHE_AGE_SEC, (60*60)).

% cached reverse lookup
gethostbyaddr(Addr) ->
    gethostbyaddr(Addr, tap_config:getconfig(reverselookup)).

gethostbyaddr(_Addr, false) ->
    "unknown";
gethostbyaddr(Addr, true) ->
    LookupFn = fun() ->
        R = case inet:gethostbyaddr(Addr) of
            {ok, #hostent{h_name = Hostname}} ->
                Hostname;
            {error, Error} ->
                lists:flatten(io_lib:format("notfound_~p", [Error]))
        end,
        list_to_binary(R)
    end,
    cache_get(Addr, LookupFn).

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

inet_parse_address(B) when is_binary(B) ->
    inet_parse_address(binary_to_list(B));
inet_parse_address(L) when is_list(L) ->
    {ok, IpAddr} = inet:parse_address(L),
    IpAddr.

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

allowquery(Query, WhiteList, BlackList) ->
    re(Query, WhiteList) andalso (not re(Query, BlackList)).

re(_, []) ->
    false;
re(Query, [RE | REList]) ->
    case re:run(Query, RE, [{capture, none}]) of
        match -> true;
        nomatch ->
            re(Query, REList)
    end.

mkre(RE) ->
    {ok, MP} = re:compile(RE),
    MP.

new_cache() ->
    ets:new(?MODULE, [named_table, set, public]),
    ok.

cache_get(Key, LookupFn) ->
    case ets:lookup(?MODULE, Key) of
        [{Key, Value, TS}] ->
            case timer:now_diff(tap_time:now(), TS) >
                                            (?MAX_CACHE_AGE_SEC * 1000000) of
                true ->
                    cache_set(Key, LookupFn);
                false ->
                    ets:insert(?MODULE, {Key, Value, tap_time:now()}),
                    Value
            end;
        [] ->
            cache_set(Key, LookupFn)
    end.

cache_set(Key, LookupFn) ->
    Value = LookupFn(),
    ets:insert(?MODULE, {Key, Value, tap_time:now()}),
    Value.
