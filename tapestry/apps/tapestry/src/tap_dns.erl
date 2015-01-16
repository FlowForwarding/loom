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

-behavior(gen_server).

-include_lib("kernel/include/inet.hrl").

-export([start_link/0,
         next_lookup/0,
         gethostbyaddr_async/2,
         gethostbyaddr/1,
         allow/3,
         mkmask/2,
         allowquery/3,
         mkre/1,
         intaddr/1,
         binaryaddr/1,
         inet_parse_address/1,
         new_cache/0,
         cache_get/2]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(MAX_CACHE_AGE_SEC, (60*60)).

-define(STATE, tap_dns_state).
-record(?STATE, {lookupqueue = queue:new()}).

%------------------------------------------------------------------------------
% API
%------------------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

next_lookup() ->
    gen_server:cast(?MODULE, lookup).

gethostbyaddr_async(Addr, Fn) ->
    gethostbyaddr_async(Addr, Fn, tap_config:getconfig(reverselookup)).

% cached reverse lookup
gethostbyaddr(Addr) ->
    gethostbyaddr(Addr, tap_config:getconfig(reverselookup)).

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


allowquery(Query, WhiteList, BlackList) ->
    re(Query, WhiteList) andalso (not re(Query, BlackList)).

mkre(RE) ->
    {ok, MP} = re:compile(RE),
    MP.

new_cache() ->
    ets:new(?MODULE, [named_table, set, public]),
    ok.

cache_get(Key, LookupFn) ->
    case cache_get(Key) of
        notfound ->
            cache_set(Key, LookupFn);
        {value, Value} ->
            Value
    end.

cache_set(Key, LookupFn) ->
    Value = LookupFn(),
    ets:insert(?MODULE, {Key, Value, tap_time:now()}),
    Value.

%------------------------------------------------------------------------------
% gen_server callbacks
%------------------------------------------------------------------------------

init([]) ->
    {ok, #?STATE{}}.

handle_call(Msg, From, State) ->
    error({no_handle_call, ?MODULE}, [Msg, From, State]).

handle_cast(lookup, State = #?STATE{lookupqueue = Q}) ->
    NewQ = case queue:out(Q) of
        {empty, NQ} ->
            NQ;
        {{value, {Addr, Fn}}, NQ} ->
            case cache_get(Addr) of
                {value, Value} ->
                    % found the value in cache:
                    % process the value and do another
                    Fn(Value),
                    next_lookup(),
                    NQ;
                notfound ->
                    % value is not cached: do lookup and
                    % wait before doing another
                    DNSLookupIntervalMillis =
                                tap_config:getconfig(reverselookupinterval),
                    {ok, _} = timer:apply_after(DNSLookupIntervalMillis,
                                                    ?MODULE, next_lookup, []),
                    Fn(gethostbyaddr(Addr)),
                    NQ
            end
    end,
    {noreply, State#?STATE{lookupqueue = NewQ}};
handle_cast({gethostbyaddr, Addr, Fn}, State = #?STATE{lookupqueue = Q}) ->
    maybe_start_lookups(queue:is_empty(Q)),
    NewQ = queue:in({Addr, Fn}, Q),
    {noreply, State#?STATE{lookupqueue = NewQ}};
handle_cast(Msg, State) ->
    error({no_handle_cast, ?MODULE}, [Msg, State]).

handle_info(Msg, State) ->
    error({no_handle_info, ?MODULE}, [Msg, State]).

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

%------------------------------------------------------------------------------
% local functions
%------------------------------------------------------------------------------

% true - queue is empty; if the queue already has entries in it
% the lookups are already running.
maybe_start_lookups(true) ->
    next_lookup();
maybe_start_lookups(false) ->
    ok.

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

re(_, []) ->
    false;
re(Query, [RE | REList]) ->
    case re:run(Query, RE, [{capture, none}]) of
        match -> true;
        nomatch ->
            re(Query, REList)
    end.

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

gethostbyaddr_async(_Addr, _Fn, false) ->
    ok;
gethostbyaddr_async(Addr, Fn, true) ->
    gen_server:cast(?MODULE, {gethostbyaddr, Addr, Fn}).

cache_get(Key) ->
    case ets:lookup(?MODULE, Key) of
        [{Key, Value, TS}] ->
            case timer:now_diff(tap_time:now(), TS) >
                                            (?MAX_CACHE_AGE_SEC * 1000000) of
                true ->
                    notfound;
                false ->
                    ets:insert(?MODULE, {Key, Value, tap_time:now()}),
                    {value, Value}
            end;
        [] ->
            notfound
    end.
