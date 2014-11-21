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

-module(tap_batch).

-behavior(gen_server).

-export([start_link/0,
         load/2,
         push_qps/0,
         parse_logfile/2]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("tap_logger.hrl").

-define(STATE, tap_batch_state).
-record(?STATE, {
        mode,
        collectors = dict:new(),
        max_collector_idle_time,
        last_qps_time,
        qps_timer,
        requester_whitelist = [],
        requester_blacklist = [],
        resolved_whitelist = [],
        resolved_blacklist = []
    }).

-define(MIN_UPDATE_TIME_MILLIS, 250).
-define(MAX_UPDATE_TIME_MILLIS, 5000).

% -----------------------------------------------------------------------------
% API
% -----------------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

load(IpAddr, Data) ->
    gen_server:cast(?MODULE, {load, IpAddr, Data}).

push_qps() ->
    gen_server:cast(?MODULE, push_qps).

% -----------------------------------------------------------------------------
% bifrost callbacks
% -----------------------------------------------------------------------------

init([]) ->
    gen_server:cast(?MODULE, start),
    State = #?STATE{
        requester_whitelist =
                        mkmasks(tap_config:getconfig(requester_whitelist)),
        requester_blacklist =
                        mkmasks(tap_config:getconfig(requester_blacklist)),
        resolved_whitelist =
                        mkmasks(tap_config:getconfig(resolved_whitelist)),
        resolved_blacklist =
                        mkmasks(tap_config:getconfig(resolved_blacklist)),
        max_collector_idle_time = tap_config:getconfig(max_collector_idle_time)
    },
    case {tap_config:is_defined(anonymized, datasources),
                    tap_config:is_defined(logfile, datasources)} of
        {true, false} ->
            {ok, State#?STATE{mode = anonymized}};
        {false, true} ->
            {ok, State#?STATE{mode = logfile}};
        {true, true} ->
            {stop, bad_config_has_both_logfile_and_anonymized_datasources}
    end.

handle_call(Msg, From, State) ->
    error({no_handle_call, ?MODULE}, [Msg, From, State]).

handle_cast(start, State) ->
    NewState = qps_timer(State),
    {noreply, NewState#?STATE{last_qps_time = tap_time:now()}, hibernate};
handle_cast({load, IpAddr, FtpFile}, State = #?STATE{mode = anonymized}) ->
    NewState = load_tar(IpAddr, FtpFile, State),
    {noreply, NewState, hibernate};
handle_cast({load, IpAddr, FtpFile}, State = #?STATE{mode = logfile}) ->
    NewState = load_logfile(IpAddr, FtpFile, State),
    {noreply, NewState, hibernate};
handle_cast(push_qps, State) ->
    State1 = expire_collectors(State),
    push_qps(State1),
    State2 = qps_timer(State1),
    {noreply, State2, hibernate};
handle_cast(Msg, State) ->
    error({no_handle_cast, ?MODULE}, [Msg, State]).

handle_info(Msg, State) ->
    error({no_handle_info, ?MODULE}, [Msg, State]).

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

% -----------------------------------------------------------------------------
% private functions
% -----------------------------------------------------------------------------

qps_timer(State = #?STATE{qps_timer = OldTimer}) ->
    timer:cancel(OldTimer), % ignore errors
    {ok, NewTimer} = timer:apply_after(?MAX_UPDATE_TIME_MILLIS,
                                                    ?MODULE, push_qps, []),
    State#?STATE{qps_timer = NewTimer}.

add_collector(_IpAddr, 0, State) ->
    State;
add_collector(IpAddr, QPS, State = #?STATE{collectors = Collectors}) ->
    NewCollectors = dict:store(IpAddr, {tap_time:now(), QPS}, Collectors),
    State#?STATE{collectors = NewCollectors}.

maybe_push_qps(#?STATE{last_qps_time = LastUpdate}) ->
    % push update if:
    %   last update was more than MIN_UPDATE_TIME_MILLIS seconds ago
    Now = tap_time:now(),
    LastUpdateAge = tap_time:diff_millis(Now, LastUpdate),
    case LastUpdateAge > ?MIN_UPDATE_TIME_MILLIS of
        true -> push_qps();
        _ -> ok
    end.

expire_collectors(State = #?STATE{max_collector_idle_time =
                                                MaxCollectorIdleTime,
                                  collectors = Collectors}) ->
    ActiveCollectors = dict:fold(
        fun(IpAddr, Value = {Time, _}, D) ->
            case tap_time:since(Time) > MaxCollectorIdleTime of
                true ->
                    ?DEBUG("Expire collector ~p", [IpAddr]),
                    D;
                false ->
                    dict:store(IpAddr, Value, D)
            end
        end, dict:new(), Collectors),
    State#?STATE{collectors = ActiveCollectors}.

push_qps(#?STATE{collectors = Collectors}) ->
    {CollectorStats, QPS} = lists:mapfoldl(
        fun({IpAddr, {Time, QPS}}, Sum) ->
            {{grid, IpAddr, tap_time:universal(Time), QPS}, Sum + QPS}
        end, 0.0, dict:to_list(Collectors)),
    tap_client_data:qps(?MODULE, QPS, CollectorStats, tap_time:universal_now()).

safe_div(_, 0) -> 0;
safe_div(N, D) -> N/D.

load_tar(IpAddr, FtpFile, State) ->
    BinaryFile = extract_file(FtpFile),
    {QPS, Data} = parse_file(BinaryFile),
    ?DEBUG("ftp tar data tar length from ~p: ~p~n",[IpAddr, length(Data)]),
    tap_ds:ordered_edges(edges(Data)),
    State1 = add_collector(IpAddr, QPS, State),
    maybe_push_qps(State1),
    State1.

load_logfile(IpAddr, FtpFile, State) ->
    FilterFn = fun(RequesterIpAddr, ResolvedIpAddr) ->
                   tap_dns:allow(RequesterIpAddr,
                       State#?STATE.requester_whitelist,
                       State#?STATE.requester_blacklist) andalso
                   tap_dns:allow(ResolvedIpAddr,
                       State#?STATE.resolved_whitelist,
                       State#?STATE.resolved_blacklist)
               end,
    {QPS, Data} = parse_zlogfile(FtpFile, FilterFn),
    ?DEBUG("ftp log data tar length from ~p: ~p~n",[IpAddr, length(Data)]),
    tap_ds:ordered_edges(edges(Data)),
    State1 = add_collector(IpAddr, QPS, State),
    maybe_push_qps(State1),
    State1.

edges(Data) ->
    [Edge || {_, Edge} <- Data].

extract_file(CompressedTarBytes)->
    case erl_tar:extract({binary, CompressedTarBytes}, [compressed, memory]) of
        {ok, Files} ->
            locate_log_file(Files);
        Error ->
            ?DEBUG("can't process tar file ~p~n", [Error]),
            bad_file
    end.

locate_log_file([]) ->
    <<>>;
locate_log_file([{FileName, Binary} | Rest]) ->
    case filename:extension(FileName) of
        ".log" -> Binary;
        _ -> locate_log_file(Rest)
    end.

parse_file(bad_file) ->
    [];
parse_file(BinaryData) ->
    parse_file(BinaryData, []).

parse_file(<<BitString:53/binary, BinaryData/binary>>, Data) ->
    <<Time:10/binary, _S:1/binary,
       ID1:20/binary, _S:1/binary,
       ID2:20/binary, _Rest/binary>> = BitString,
    Interaction = {ID1, anonymous, ID2, anonymous},
    parse_file(BinaryData, [{Time, Interaction} | Data]);
parse_file(_BinaryData, Atad) ->
    Data = lists:reverse(Atad),
    [{StartTime, _, _}|_] = Data,
    [{EndTime, _, _}|_] = Atad,
    TimeDiff = binary_to_integer(EndTime) - binary_to_integer(StartTime),
    QPS = safe_div(length(Data), TimeDiff),
    {QPS, Data}.

parse_zlogfile(ZBin, FilterFn) ->
    Bin = safe_gunzip(ZBin),
    Data = parse_logfile(Bin, FilterFn),
    % extract first and last datetime
    [{StartTime, _, _}|_] = Data,
    {EndTime, _, _} = lists:last(Data),
    TimeDiff = tap_time:universal_time_diff(
                    parse_timestamp(StartTime),
                    parse_timestamp(EndTime)),
    QPS = safe_div(length(Data), TimeDiff),
    {QPS, Data}.

parse_logfile(Bin, FilterFn) ->
    % match log records:
    % ipv4 example:
    %   15-May-2014 13:33:18.468 client 192.168.11.172#50276: view
    %   8: UDP: query: p14-keyvalueservice.icloud.com IN A response:
    %   NOERROR + p14-keyvalueservice.icloud.com. 86400 IN CNAME
    %   p14-keyvalueservice.icloud.com.akadns.net.;
    %   p14-keyvalueservice.icloud.com.akadns.net. 120 IN A 17.151.226.32;
    %   p14-keyvalueservice.icloud.com.akadns.net. 120 IN A 17.151.226.33;

    % ipv6 examples:
    %   15-May-2014 13:33:26.049 client 192.168.11.130#49974: view
    %   8: UDP: query: www.isg-apple.com.akadns.net IN AAAA response:
    %   NOERROR + www.isg-apple.com.akadns.net. 27 IN CNAME
    %   www.apple.com.edgekey.net.; www.apple.com.edgekey.net. 465 IN
    %   CNAME e3191.dscc.akamaiedge.net.; e3191.dscc.akamaiedge.net.
    %   20 IN AAAA 2001:418:142a:194::c77; e3191.dscc.akamaiedge.net.
    %   20 IN AAAA 2001:418:142a:19d::c77; e3191.dscc.akamaiedge.net.
    %   20 IN AAAA 2001:418:142a:18e::c77;
    %
    %   29-Oct-2014 09:48:02.588 client 2620:10a:6000:2000::2c6#7908:
    %   UDP: query: daisy.ubuntu.com IN A response: NOERROR + daisy.ubuntu.com.
    %   339 IN A 91.189.92.55; daisy.ubuntu.com. 339 IN A 91.189.92.57;
    %
    %   29-Oct-2014 09:48:06.309 client 2620:10a:6000:2000::28c#23959:
    %   UDP: query: outlook.infoblox.com IN A response: NOERROR +A
    %   outlook.infoblox.com. 10 IN CNAME casarray1.infoblox.com.;
    %   casarray1.infoblox.com. 10 IN A 10.120.3.104;

    Matches = case re:run(Bin,"(..-...-.... ..:..:......) client ((?:[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3})|(?:[:a-f0-9]+)).* UDP: query: (.*) IN A+ response: NOERROR .*? IN A+ ((?:[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3})|(?:[:a-f0-9]+));", [global, {capture,[1,3,2,4],binary}]) of
        {match, M} -> M;
        _ -> []
    end,
    % convert addresses to the tuple format. Storing the binaries directly
    % results in references to the larger binary that impedes gc of the
    % binry heap.  Could also binary:copy/1.  Decoding addresses
    % This makes the batch processing more consistent with the packet_in
    % processing.
    lists:reverse(lists:foldl(
        fun([Timestamp, Query, Requester, Resolved], L) ->
            RequesterIpAddr = inet_parse_address(Requester),
            ResolvedIpAddr = inet_parse_address(Resolved),
            case FilterFn(RequesterIpAddr, ResolvedIpAddr) of
                false ->
                    L;
                true ->
                    [{Timestamp,
                      tap_ds:endpoint(RequesterIpAddr,
                                            tap_dns:gethostbyaddr(Requester)),
                      tap_ds:endpoint(ResolvedIpAddr, Query)} | L]
            end
        end, [], Matches)).

parse_timestamp(TimestampB) ->
    {ok,[Day, Month, Year, Hour, Min, Sec],_} =
        io_lib:fread("~2d-~3s-~4d ~2d:~2d:~2d", binary_to_list(TimestampB)),
    {{Year, parse_month(Month), Day},{Hour, Min, Sec}}.

parse_month("Jan") -> 1;
parse_month("Feb") -> 2;
parse_month("Mar") -> 3;
parse_month("Apr") -> 4;
parse_month("May") -> 5;
parse_month("Jun") -> 6;
parse_month("Jul") -> 7;
parse_month("Aug") -> 8;
parse_month("Sep") -> 9;
parse_month("Oct") -> 10;
parse_month("Nov") -> 11;
parse_month("Dec") -> 12.

inet_parse_address(B) when is_binary(B) ->
    inet_parse_address(binary_to_list(B));
inet_parse_address(L) when is_list(L) ->
    {ok, IpAddr} = inet:parse_address(L),
    IpAddr.

safe_gunzip(ZBin) ->
    try
        zlib:gunzip(ZBin)
    catch
        error:data_error -> <<>>
    end.

mkmasks(MaskList) ->
    [tap_dns:mkmask(inet_parse_address(Addr), Length) ||
                                            {Addr, Length} <- MaskList].
