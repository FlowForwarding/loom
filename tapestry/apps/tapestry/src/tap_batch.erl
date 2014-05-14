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
        collectors = dict:new(),
        total_count = 0,
        last_qps_time,
        qps_update_interval,
        qps_timer
    }).

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
    {ok, #?STATE{}}.

handle_call(Msg, From, State) ->
    error({no_handle_call, ?MODULE}, [Msg, From, State]).

handle_cast(start, State) ->
    NewState = qps_timer(
                    State#?STATE{last_qps_time = tap_time:now(),
                    qps_update_interval = qps_update_interval()}),
    {noreply, NewState};
handle_cast({load, IpAddr, FtpFile}, State) ->
    BinaryFile = extract_file(FtpFile),
    Data = parse_file(BinaryFile),
    ?DEBUG("ftp data length from ~p: ~p~n",[IpAddr, length(Data)]),
    tap_ds:ordered_edges(Data),
    NewState = add_collector(IpAddr, length(Data), State),
    {noreply, NewState};
handle_cast(push_qps, State) ->
    NewState = push_qps(State),
    {noreply, NewState};
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

qps_update_interval() ->
    {seconds, Time} = tap_config:getconfig(qps_max_interval),
    Time.

qps_timer(State = #?STATE{qps_update_interval = After, qps_timer = OldTimer}) ->
    timer:cancel(OldTimer), % ignore errors
    {ok, NewTimer} = timer:apply_after(After*1000, ?MODULE, push_qps, []),
    State#?STATE{qps_timer = NewTimer}.

add_collector(IpAddr, Count, State = #?STATE{
                                        total_count = TCount,
                                        collectors = Collectors}) ->
    NewCollectors = dict:store(IpAddr, {tap_time:now(), Count}, Collectors),
    State#?STATE{total_count = TCount + Count, collectors = NewCollectors}.

push_qps(State = #?STATE{
                    collectors = Collectors,
                    total_count = TCount,
                    last_qps_time = TTime}) ->
    Now = tap_time:now(),
    CollectorStats = [{grid, IpAddr, per_sec(Count, Time)} ||
                        {IpAddr, {Time, Count}} <- dict:to_list(Collectors)],
    tap_client_data:qps(per_sec(TCount, TTime), CollectorStats,
                                                    tap_time:universal(Now)),
    State#?STATE{total_count = 0, last_qps_time = Now}.
        
per_sec(Count, LastTime) ->
    Count / tap_time:since(LastTime).

extract_file(CompressedTarBytes)->
    {ok, Files} =
        erl_tar:extract({binary, CompressedTarBytes}, [compressed, memory]),
    locate_log_file(Files).

locate_log_file([]) ->
    <<>>;
locate_log_file([{FileName, Binary} | Rest]) ->
    case filename:extension(FileName) of
        ".log" -> Binary;
        _ -> locate_log_file(Rest)
    end.

parse_file(BinaryData) ->
    parse_file(BinaryData, []).

parse_file(<<BitString:53/binary, BinaryData/binary>>, Data) ->
    <<_Time:10/binary, _S:1/binary,
       ID1:20/binary, _S:1/binary,
       ID2:20/binary, _Rest/binary>> = BitString,
    V1 = bitstring_to_list(ID1),
    V2 = bitstring_to_list(ID2),
    Interaction = {V1, V2},
    parse_file(BinaryData, [Interaction | Data]);
parse_file(_BinaryData, Data)->
    lists:reverse(Data).
