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
%% @author Ryan Crum <ryan.j.crum@gmail.com>, Infoblox Inc <info@infoblox.com>
%% @copyright 2013 Infoblox Inc
%% @doc FTP service to connect Tapestry to the Infoblox Grid (6.9 or later).  The code is based on 
%% memory_server.erl example from Ryan Crum's bifrost Erlang based ftp server


-module(tap_batch).

-behavior(gen_server).

-export([start_link/0,
         load/2,
         push_qps/0]).

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
        total_count = 0,
        last_qps_time,
        qps_update_interval,
        qps_timer,
        last_qps = 0.0
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
    Mode = tap_config:getconfig(datasource),
    gen_server:cast(?MODULE, start),
    {ok, #?STATE{mode = Mode}}.

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
    State1 = push_qps(State),
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
add_collector(IpAddr, Count, State = #?STATE{
                                        total_count = TCount,
                                        collectors = Collectors}) ->
    NewCollectors = dict:store(IpAddr, {tap_time:now(), Count}, Collectors),
    State#?STATE{total_count = TCount + Count, collectors = NewCollectors}.

maybe_push_qps(#?STATE{last_qps_time = LastUpdate}) ->
    % push update if:
    %   last update was more than MIN_UPDATE_TIME_MILLIS seconds ago
    Now = tap_time:now(),
    LastUpdateAge = tap_time:diff_millis(Now, LastUpdate),
    case LastUpdateAge > ?MIN_UPDATE_TIME_MILLIS of
        true -> push_qps();
        _ -> ok
    end.

push_qps(State = #?STATE{collectors = Collectors}) ->
    CollectorStats = [{grid, IpAddr, per_sec(Count, Time)} ||
                        {IpAddr, {Time, Count}} <- dict:to_list(Collectors)],
    Now = tap_time:now(),
    {QPS, NewState} = update_qps(State),
    tap_client_data:qps(QPS, CollectorStats, tap_time:universal(Now)),
    NewState.

update_qps(State = #?STATE{
                        total_count = TCount,
                        last_qps_time = TTime,
                        last_qps = LastQPS}) ->
    CurrentQPS = per_sec(TCount, TTime),
    case CurrentQPS == 0 of
        true ->
            {LastQPS, State};
        _ ->
            {CurrentQPS, State#?STATE{total_count = 0,
                                      last_qps_time = tap_time:now(),
                                      last_qps = CurrentQPS}}
    end.

per_sec(Count, LastTime) ->
    safe_div(Count, tap_time:since(LastTime)).

safe_div(_, 0) -> 0;
safe_div(N, D) -> N/D.

load_tar(IpAddr, FtpFile, State) ->
    BinaryFile = extract_file(FtpFile),
    Data = parse_file(BinaryFile),
    ?DEBUG("ftp tar data tar length from ~p: ~p~n",[IpAddr, length(Data)]),
    tap_ds:ordered_edges(Data),
    State1 = add_collector(IpAddr, length(Data), State),
    maybe_push_qps(State1),
    State1.

load_logfile(IpAddr, FtpFile, State) ->
    Data = parse_logfile(FtpFile),
    ?DEBUG("ftp log data tar length from ~p: ~p~n",[IpAddr, length(Data)]),
    tap_ds:ordered_edges(Data),
    State1 = add_collector(IpAddr, length(Data), State),
    maybe_push_qps(State1),
    State1.

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
    <<_Time:10/binary, _S:1/binary,
       ID1:20/binary, _S:1/binary,
       ID2:20/binary, _Rest/binary>> = BitString,
    Interaction = {ID1, ID2},
    parse_file(BinaryData, [Interaction | Data]);
parse_file(_BinaryData, Data)->
    lists:reverse(Data).

parse_logfile(ZBin) ->
    Bin =  safe_gunzip(ZBin),
    Matches = case re:run(Bin,"client (.*)#.* UDP: query: (.*) IN A response: NOERROR.*? ([0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3});", [global, {capture,[2,1,3],binary}]) of
        {match, M} -> M;
        _ -> []
    end,
    % convert addresses to the tuple format. Storing the binaries directly
    % results in references to the larger binary that impedes gc of the
    % binry heap.  Could also binary:copy/1.  This makes the batch
    % processing more consistent with the packet_in processing.
    [{inet_parse_address(Requester), inet_parse_address(Resolved)} ||
                                [_Query, Requester, Resolved] <- Matches].

inet_parse_address(B) ->
    {ok, IpAddr} = inet:parse_address(binary_to_list(B)),
    IpAddr.

safe_gunzip(ZBin) ->
    try
        zlib:gunzip(ZBin)
    catch
        error:data_error -> <<>>
    end.
