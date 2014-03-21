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

%%% @doc
%%% Publish OpenFlow stats into folsom.
%%% @end
-module(simple_ne_folsom).

-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(STATE, simple_ne_folsom_state).

-include_lib("loom/include/loom_logger.hrl").
-include_lib("of_protocol/include/of_protocol.hrl").
-include("simple_ne_ofsh.hrl").

-record(?STATE, {
    stats :: term(),
    stats_version = 0 :: integer()
}).

-record(flow_stat, {
    table_id :: integer(),
    priority :: binary(),
    cookie :: binary(),
    duration_sec :: integer(),
    duration_nsec :: integer(),
    byte_count :: integer(),
    packet_count :: integer()
}).

-record(table_stat, {
    table_id :: integer(),
    active_count :: integer(),
    lookup_count :: integer(),
    matched_count :: integer()
}).

-record(aggregate_stat, {
    packet_count :: integer(),
    byte_count :: integer(),
    flow_count :: integer()
}).

-record(port_stat, {
    port_no :: integer(),
    rx_packets :: integer(),
    tx_packets :: integer(),
    rx_bytes :: integer(),
    tx_bytes :: integer(),
    rx_dropped :: integer(),
    tx_dropped :: integer(),
    rx_errors :: integer(),
    tx_errors :: integer(),
    rx_frame_err :: integer(),
    rx_over_err :: integer(),
    rx_crc_err :: integer(),
    collisions :: integer(),
    duration_sec :: integer(),
    duration_nsec :: integer()
}).

-record(queue_stat, {
    port_no :: integer(),
    queue_id :: integer(),
    tx_bytes :: integer(),
    tx_packets :: integer(),
    tx_errors :: integer(),
    duration_sec :: integer(),
    duration_nsec :: integer()
}).

-record(group_stat, {
    group_id :: integer(),
    ref_count :: integer(),
    packet_count :: integer(),
    byte_count :: integer(),
    duration_sec :: integer(),
    duration_nsec :: integer(),
    bucket_stats :: [bucket_stat()]
}).

-record(bucket_stat, {
    bucket_id :: integer(),
    packet_count :: integer(),
    byte_count :: integer()
}).
-type bucket_stat() :: #bucket_stat{}.

-record(meter_stat, {
    meter_id :: integer(),
    flow_count :: integer(),
    packet_in_count :: integer(),
    byte_in_count :: integer(),
    duration_sec :: integer(),
    duration_nsec :: integer(),
    band_stats :: [band_stat()]
}).

-record(band_stat, {
    band_id :: integer(),
    packet_band_count :: integer(),
    byte_band_count :: integer()
}).
-type band_stat() :: #band_stat{}.

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0,
         handle_message/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, noargs, []).

-spec handle_message(ofp_message(), ofs_state()) -> ok.
handle_message(Msg, #?OFS_STATE{datapath_id = DatapathId}) ->
    gen_server:cast(?SERVER, {handle_message, DatapathId, Msg}),
    ok.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(noargs) ->
    State = #?STATE{stats = gb_trees:empty()},
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({handle_message, DatapathId, Msg}, State) ->
    Metrics = lists:flatten(process_reply(DatapathId, Msg)),
    CoreName = folsom_corename(DatapathId, Msg),
    NewState = update_folsom(CoreName, Metrics, State),
    {noreply, NewState};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

update_folsom(CoreName, Metrics,
                        State = #?STATE{stats = ExistingMetrics,
                                        stats_version = Version}) ->
    % create folsom metrics as needed and set value
    NewVersion = Version + 1,
    Metrics1 = lists:foldl(
        fun(Metric, Existing) ->
            {Name, Value} = Metric,
            FullName = list_to_binary([CoreName, Name]),
            NewExisting = maybe_create_metric(FullName, Existing, NewVersion),
            ok = folsom_metrics:notify(FullName, Value),
            NewExisting
         end, ExistingMetrics, Metrics),
    % delete folsom metrics with the same corename that were not updated
    Metrics2 = lists:foldl(
        fun({N, V}, M) ->
            case V /= NewVersion andalso common_prefix(CoreName, N) of
                true ->
                    folsom_metrics:delete_metric(N),
                    gb_trees:delete(N, M);
                false ->
                    M
            end
        end, Metrics1, gb_trees:to_list(Metrics1)),
    State#?STATE{stats = Metrics2, stats_version = NewVersion}.

common_prefix(Prefix, Value) ->
    binary:longest_common_prefix([Prefix, Value]) == size(Prefix).

% create the folsom metric if it doesn't exist yet
maybe_create_metric(Name, Existing, NewVersion) ->
    case gb_trees:lookup(Name, Existing) of
        none ->
            folsom_metrics:new_gauge(Name);
        _ ->
            ok
    end,
    gb_trees:enter(Name, NewVersion, Existing).

process_reply(_DatapathId, {flow_stats_reply, _Xid, Body}) ->
    Flows = proplists:get_value(flows, Body),
    process_flows(Flows);
process_reply(_DatapathId, {table_stats_reply, _Xid, Body}) ->
    Tables = proplists:get_value(tables, Body),
    process_tables(Tables);
process_reply(_DatapathId, {aggregate_stats_reply, _Xid, Body}) ->
    process_aggregates(Body);
process_reply(_DatapathId, {port_stats_reply, _Xid, Body}) ->
    Ports = proplists:get_value(ports, Body),
    process_ports(Ports);
process_reply(_DatapathId, {queue_stats_reply, _Xid, Body}) ->
    Queues = proplists:get_value(queues, Body),
    process_queues(Queues);
process_reply(_DatapathId, {group_stats_reply, _Xid, Body}) ->
    Groups = proplists:get_value(groups, Body),
    process_groups(Groups);
process_reply(_DatapathId, {meter_stats_reply, _Xid, Body}) ->
    Meters = proplists:get_value(meters, Body),
    process_meters(Meters);
process_reply(DatapathId, {UnknownReply, _Xid, _Body}) ->
    ?INFO("Unknown stats poll reply: ~p(~p)~n", [DatapathId, UnknownReply]),
    [].

% flow stats

process_flows(undefined) ->
    [];
process_flows(Flows) ->
    lists:foldl(
        fun (Flow, Metrics) ->
            [make_stats(process_flow(Flow)) | Metrics]
        end, [], Flows).

process_flow(Flow) ->
    lists:foldl(
        fun ({table_id, TableId}, Stat) ->
                Stat#flow_stat{table_id = TableId};
            ({priority, Priority}, Stat) ->
                Stat#flow_stat{priority = Priority};
            ({cookie, Cookie}, Stat) ->
                Stat#flow_stat{cookie = Cookie};
            ({packet_count, PCount}, Stat) ->
                Stat#flow_stat{packet_count = PCount};
            ({byte_count, BCount}, Stat) ->
                Stat#flow_stat{byte_count = BCount};
            ({duration_sec, Duration}, Stat) ->
                Stat#flow_stat{duration_sec = Duration};
            ({duration_nsec, Duration}, Stat) ->
                Stat#flow_stat{duration_nsec = Duration};
            (_, Stat) ->
                Stat
        end, #flow_stat{}, Flow).

% table stats

process_tables(undefined) ->
    [];
process_tables(Tables) ->
    lists:foldl(
        fun (Table, Metrics) ->
                [make_stats(process_table(Table)) | Metrics]
        end, [], Tables).

process_table(Table) ->
    lists:foldl(
        fun ({table_id, TableId}, Stat) ->
                Stat#table_stat{table_id = TableId};
            ({active_count, Count}, Stat) ->
                Stat#table_stat{active_count = Count};
            ({lookup_count, Count}, Stat) ->
                Stat#table_stat{lookup_count = Count};
            ({matched_count, Count}, Stat) ->
                Stat#table_stat{matched_count = Count};
            (_, Stat) ->
                Stat
        end, #table_stat{}, Table).

% aggregate stats

process_aggregates(undefined) ->
    [];
process_aggregates(Body) ->
    make_stats(process_aggregate(Body)).

process_aggregate(Aggregate) ->
    lists:foldl(
        fun ({packet_count, PCount}, Stat) ->
                Stat#aggregate_stat{packet_count = PCount};
            ({byte_count, BCount}, Stat) ->
                Stat#aggregate_stat{byte_count = BCount};
            ({flow_count, FCount}, Stat) ->
                Stat#aggregate_stat{flow_count = FCount};
            (_, Stat) ->
                Stat
        end, #aggregate_stat{}, Aggregate).

% port stats

process_ports(undefined) ->
    [];
process_ports(Ports) ->
    lists:foldl(
        fun (Port, Metrics) ->
                [make_stats(process_port(Port)) | Metrics]
        end, [], Ports).

process_port(Port) ->
    lists:foldl(
        fun ({port_no, PortNo}, Stat) ->
                Stat#port_stat{port_no = PortNo};
            ({rx_packets, RPacket}, Stat) ->
                Stat#port_stat{rx_packets = RPacket};
            ({tx_packets, TPacket}, Stat) ->
                Stat#port_stat{tx_packets = TPacket};
            ({rx_bytes, RByte}, Stat) ->
                Stat#port_stat{rx_bytes = RByte};
            ({tx_bytes, TByte}, Stat) ->
                Stat#port_stat{tx_bytes = TByte};
            ({rx_dropped, RDrop}, Stat) ->
                Stat#port_stat{rx_dropped = RDrop};
            ({tx_dropped, TDrop}, Stat) ->
                Stat#port_stat{tx_dropped = TDrop};
            ({rx_errors, RError}, Stat) ->
                Stat#port_stat{rx_errors = RError};
            ({tx_errors, TError}, Stat) ->
                Stat#port_stat{tx_errors = TError};
            ({rx_frame_err, RFrame}, Stat) ->
                Stat#port_stat{rx_frame_err = RFrame};
            ({rx_over_err, ROver}, Stat) ->
                Stat#port_stat{rx_over_err = ROver};
            ({rx_crc_err, RCRC}, Stat) ->
                Stat#port_stat{rx_crc_err = RCRC};
            ({collisions, TCollision}, Stat) ->
                Stat#port_stat{collisions = TCollision};
            ({duration_sec, Duration}, Stat) ->
                Stat#port_stat{duration_sec = Duration};
            ({duration_nsec, NDuration}, Stat) ->
                Stat#port_stat{duration_nsec = NDuration};
            (_, Stat) ->
                Stat
        end, #port_stat{}, Port).

% queue stats

process_queues(undefined) ->
    [];
process_queues(Queues) ->
    lists:foldl(
        fun(Queue, Metrics) ->
            [make_stats(process_queue(Queue)) | Metrics]
        end, [], Queues).

process_queue(Queue) ->
    lists:foldl(
        fun ({port_no, Port}, Stat) ->
                Stat#queue_stat{port_no = Port};
            ({tx_packets, TPacket}, Stat) ->
                Stat#queue_stat{tx_packets = TPacket};
            ({tx_bytes, TByte}, Stat) ->
                Stat#queue_stat{tx_bytes = TByte};
            ({tx_errors, TError}, Stat) ->
                Stat#queue_stat{tx_errors = TError};
            ({duration_sec, Duration}, Stat) ->
                Stat#queue_stat{duration_nsec = Duration};
            ({duration_nsec, NDuration}, Stat) ->
                Stat#queue_stat{duration_nsec = NDuration};
            (_, Stat) ->
                Stat
        end, #queue_stat{}, Queue).

% group stats

process_groups(undefined) ->
    [];
process_groups(Groups) ->
    lists:foldl(
        fun (Group, Metrics) ->
            [make_stats(process_group(Group)) | Metrics]
        end, [], Groups).

process_group(Group) ->
    lists:foldl(
        fun ({group_id, GroupId}, Stat) ->
                Stat#group_stat{group_id = GroupId};
            ({ref_count, RCount}, Stat) ->
                Stat#group_stat{ref_count = RCount};
            ({packet_count, PCount}, Stat) ->
                Stat#group_stat{packet_count = PCount};
            ({byte_count, BCount}, Stat) ->
                Stat#group_stat{byte_count = BCount};
            ({duration_sec, Duration}, Stat) ->
                Stat#group_stat{duration_nsec = Duration};
            ({duration_nsec, NDuration}, Stat) ->
                Stat#group_stat{duration_nsec = NDuration};
            ({bucket_stats, BucketStats}, Stat) ->
                Stat#group_stat{bucket_stats = process_buckets(BucketStats)};
            (_, Stat) -> Stat
        end, #group_stat{}, Group).

process_buckets(Buckets) ->
    {_, ProcessedBuckets} = lists:foldl(
        fun (Bucket, {Count, Stats}) ->
                {Count + 1, [process_bucket(Count, Bucket) | Stats]}
        end, {0, []}, Buckets),
    ProcessedBuckets.

process_bucket(BucketId, Bucket) ->
    lists:foldl(
        fun ({packet_count, PCount}, Stat) ->
                Stat#bucket_stat{packet_count = PCount};
            ({byte_count, BCount}, Stat) ->
                Stat#bucket_stat{byte_count = BCount};
            (_, Stat) -> Stat
        end, #bucket_stat{bucket_id = BucketId}, Bucket).

% meter stats

process_meters(undefined) ->
    [];
process_meters(Meters) ->
    lists:foldl(
        fun (Meter, Metrics) ->
            [make_stats(process_meter(Meter)) | Metrics]
        end, [], Meters).

process_meter(Meter) ->
    lists:foldl(
        fun ({meter_id, MeterId}, Stat) ->
                Stat#meter_stat{meter_id = MeterId};
            ({flow_count, FCount}, Stat) ->
                Stat#meter_stat{flow_count = FCount};
            ({packet_in_count, PCount}, Stat) ->
                Stat#meter_stat{packet_in_count = PCount};
            ({byte_in_count, BCount}, Stat) ->
                Stat#meter_stat{byte_in_count = BCount};
            ({duration_sec, Duration}, Stat) ->
                Stat#meter_stat{duration_nsec = Duration};
            ({duration_nsec, NDuration}, Stat) ->
                Stat#meter_stat{duration_nsec = NDuration};
            ({band_stats, BandStats}, Stat) ->
                Stat#meter_stat{band_stats = process_bands(BandStats)};
            (_, Stat) -> Stat
        end, #meter_stat{}, Meter).

process_bands(Bands) ->
    {_, ProcessedBands} = lists:foldl(
        fun (Band, {Count, Stats}) ->
                {Count + 1, [process_band(Count, Band) | Stats]}
        end, {0, []}, Bands),
    ProcessedBands.

process_band(BandId, Band) ->
    lists:foldl(
        fun ({packet_band_count, PCount}, Stat) ->
                Stat#band_stat{packet_band_count = PCount};
            ({byte_band_count, BCount}, Stat) ->
                Stat#band_stat{byte_band_count = BCount};
            (_, Stat) -> Stat
        end, #band_stat{band_id = BandId}, Band).

% folsom stats

make_stats(Stat) ->
    BaseName = folsom_basename(Stat),
    [{folsom_name(BaseName, Key), Value} || {Key, Value} <- folsom_stats(Stat)].

folsom_name(BaseName, Name) ->
    list_to_binary([BaseName, Name]).

folsom_corename(DatapathId, {flow_stats_reply, _, _}) ->
    list_to_binary([datapathid_to_binary(DatapathId),
     <<"-flow-">>]);
folsom_corename(DatapathId, {table_stats_reply, _, _}) ->
    list_to_binary([datapathid_to_binary(DatapathId),
     <<"-table-">>]);
folsom_corename(DatapathId, {aggregate_stats_reply, _, _}) ->
    list_to_binary([datapathid_to_binary(DatapathId),
     <<"-aggregate-">>]);
folsom_corename(DatapathId, {port_stats_reply, _, _}) ->
    list_to_binary([datapathid_to_binary(DatapathId),
     <<"-port-">>]);
folsom_corename(DatapathId, {queue_stats_reply, _, _}) ->
    list_to_binary([datapathid_to_binary(DatapathId),
     <<"-queue-">>]); 
folsom_corename(DatapathId, {group_stats_reply, _, _}) ->
    list_to_binary([datapathid_to_binary(DatapathId),
     <<"-group-">>]);
folsom_corename(DatapathId, {meter_stats_reply, _, _}) ->
    list_to_binary([datapathid_to_binary(DatapathId),
     <<"-meter-">>]);
folsom_corename(_DatapathId, _) ->
    <<>>.

folsom_basename(#flow_stat{table_id = TableId,
                                              priority = Priority,
                                              cookie = Cookie}) ->
    % XXX may need to identify flow by match
    [integer_to_binary(TableId), $-,
     integer_to_binary(Priority), $-,
     binary_to_hex(Cookie), $-];
folsom_basename(#table_stat{table_id = TableId}) ->
    [integer_to_binary(TableId), $-];
folsom_basename(#aggregate_stat{}) ->
    <<>>;
folsom_basename(#port_stat{port_no = Port}) ->
    [integer_to_binary(Port), $-];
folsom_basename(#queue_stat{port_no = Port}) ->
    % XXX include queue id
    [integer_to_binary(Port), $-];
folsom_basename(#group_stat{group_id = Group}) ->
    [integer_to_binary(Group), $-];
folsom_basename(#meter_stat{meter_id = Meter}) ->
    [integer_to_binary(Meter), $-].

folsom_stats(Stat = #flow_stat{}) ->
    #flow_stat{duration_sec = SDuration,
               duration_nsec = NSDuration,
               packet_count = PCount,
               byte_count = BCount} = Stat,
    [{<<"duration_sec">>, SDuration},
     {<<"duration_nsec">>, NSDuration},
     {<<"packet_count">>, PCount},
     {<<"byte_count">>, BCount}];
folsom_stats(Stat = #table_stat{}) ->
    #table_stat{active_count = ACount,
                lookup_count = LCount,
                matched_count = MCount} = Stat,
    [{<<"active_count">>, ACount},
     {<<"lookup_count">>, LCount},
     {<<"matched_count">>, MCount}];
folsom_stats(Stat = #aggregate_stat{}) ->
    #aggregate_stat{packet_count = PCount,
                    byte_count = BCount,
                    flow_count = FCount} = Stat,
    [{<<"packet_count">>, PCount},
     {<<"byte_count">>, BCount},
     {<<"flow_count">>, FCount}];
folsom_stats(Stat = #port_stat{}) ->
    #port_stat{rx_packets = RPacket,
               tx_packets = TPacket,
               rx_bytes = RByte,
               tx_bytes = TByte,
               rx_dropped = RDrop,
               tx_dropped = TDrop,
               rx_errors = RError,
               tx_errors = TError,
               rx_frame_err = RFramed,
               rx_over_err = ROver,
               rx_crc_err = RCRC,
               collisions = Collusions,
               duration_sec = Duration,
               duration_nsec = NDuration} = Stat,
    [{<<"rx_packets">>, RPacket},
     {<<"tx_packets">>, TPacket},
     {<<"rx_bytes">>, RByte},
     {<<"tx_bytes">>, TByte},
     {<<"rx_dropped">>, RDrop},
     {<<"tx_dropped">>, TDrop},
     {<<"rx_errors">>, RError},
     {<<"tx_errors">>, TError},
     {<<"rx_frame_err">>, RFramed},
     {<<"rx_over_err">>, ROver},
     {<<"rx_crc_err">>, RCRC},
     {<<"collisions">>, Collusions},
     {<<"duration_sec">>, Duration},
     {<<"duration_nsec">>, NDuration}];
folsom_stats(Stat = #queue_stat{}) ->
    #queue_stat{tx_packets = TPacket,
                tx_bytes = TByte,
                tx_errors = TError,
                duration_sec = Duration,
                duration_nsec = NDuration} = Stat,
    [{<<"tx_packets">>, TPacket},
     {<<"tx_bytes">>, TByte},
     {<<"tx_errors">>, TError},
     {<<"duration_sec">>, Duration},
     {<<"duration_nsec">>, NDuration}];
folsom_stats(Stat = #group_stat{}) ->
    #group_stat{ref_count = RCount,
                packet_count = PCount,
                byte_count = BCount,
                duration_sec = Duration,
                duration_nsec = NDuration,
                bucket_stats = Buckets} = Stat,
    lists:flatten([{<<"packet_count">>, PCount},
     {<<"byte_count">>, BCount},
     {<<"ref_count">>, RCount},
     {<<"duration_sec">>, Duration},
     {<<"duration_nsec">>, NDuration},
     [folsom_stats(Bucket) || Bucket <- Buckets]]);
folsom_stats(Stat = #bucket_stat{}) ->
    #bucket_stat{bucket_id = BucketId,
                 packet_count = PCount,
                 byte_count = BCount} = Stat,
    Base = [<<"bucket-">>, integer_to_list(BucketId), $-],
    [{[Base, <<"packet_count">>], PCount},
     {[Base, <<"byte_count">>], BCount}];
folsom_stats(Stat = #meter_stat{}) ->
    #meter_stat{flow_count = FCount,
                packet_in_count = PCount,
                byte_in_count = BCount,
                duration_sec = Duration,
                duration_nsec = NDuration,
                band_stats = Bands} = Stat,
    lists:flatten([{<<"flow_count">>, FCount},
     {<<"byte_in_count">>, BCount},
     {<<"packet_in_count">>, PCount},
     {<<"duration_sec">>, Duration},
     {<<"duration_nsec">>, NDuration},
     [folsom_stats(Band) || Band <- Bands]]);
folsom_stats(Stat = #band_stat{}) ->
    #band_stat{band_id = BandId,
               packet_band_count = PCount,
               byte_band_count = BCount} = Stat,
    Base = [<<"band-">>, integer_to_list(BandId), $-],
    [{[Base, <<"packet_band_count">>], PCount},
     {[Base, <<"byte_band_count">>], BCount}].

datapathid_to_binary({I, B}) ->
    [integer_to_binary(I), $:, binary_to_hex(B)].

binary_to_hex(Bin) when is_binary(Bin) ->
    [byte_to_hex(N1, N2) || <<N1:4, N2:4>> <= Bin].

byte_to_hex(N1, N2) ->
    [erlang:integer_to_list(N1, 16), erlang:integer_to_list(N2, 16)].
