%%%-------------------------------------------------------------------
%%% @copyright (C) 1999-2013, Erlang Solutions Ltd
%%% @author Marc Sugiyama <marc.sugiyama@erlang-solutions.com>
%%% @doc
%%% Simple network exeuctive open flow stats poller.
%%% @end
%%%-------------------------------------------------------------------
-module(simple_ne_stats).
-copyright("2013, Erlang Solutions Ltd.").

-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(STATE, simple_ne_stats_state).

-include_lib("loom/include/loom_logger.hrl").

-define(DEFAULT_STATS_INTERVAL, 10).
-define(DEFAULT_STATS, [flow, table, aggregate, port, queue, group, meter]).

-record(?STATE, {
    stats,
    stats_interval, 
    interval_timer,
    of_version,
    datapath_id
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

% XXX subscribe to all replies

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/2,
         handle_message/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Version, DatapathId) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Version, DatapathId], []).

handle_message(Msg, State) ->
    % datapath id from State
    DatapathId = '1234',
    gen_server:cast(?SERVER, {handle_message, DatapathId, Msg}),
    ok.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Version, DatapathId]) ->
    Stats = application:get_env(loom, stats, ?DEFAULT_STATS),
    StatsInterval = application:get_env(loom, stats_interval_sec, ?DEFAULT_STATS_INTERVAL),
    IntervalTimer = timer:send_interval(StatsInterval * 1000, poll_stats),
    State = #?STATE{stats = Stats,
                    stats_interval = StatsInterval,
                    interval_timer = IntervalTimer,
                    of_version = Version,
                    datapath_id = DatapathId},
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({handle_message, DatapathId, Msg}, State) ->
    % TODO - list of old stats
    update_folsom(process_reply(DatapathId, Msg)),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(poll_stats, State = #?STATE{of_version = Version,
                                        datapath_id = DatapathId,
                                        stats = Stats}) ->
    ok = poll_stats(Version, Stats, DatapathId),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

update_folsom({flows, Stats}) ->
    ok.

poll_stats(Version, Stats, DatapathId) ->
    Requests = stats_msgs(Stats, Version),
    ok = ofs_handler:send_list(DatapathId, Requests).

stats_msgs(Version, Stats) ->
    [stats_msg(Version, Stat) || Stat <- Stats].

stats_msg(Version, flow) ->
    of_msg_lib:get_flow_statistics(Version, all, [], []);
stats_msg(Version, table) ->
    of_msg_lib:get_table_stats(Version);
stats_msg(Version, aggregate) ->
    of_msg_lib:get_aggregate_statistics(Version, all, [], []);
stats_msg(Version, port) ->
    of_msg_lib:get_port_statistics(Version, all);
stats_msg(Version, queue) ->
    of_msg_lib:get_queue_statistics(Version, any, all);
stats_msg(Version, group) ->
    of_msg_lib:get_group_statistics(Version, all);
stats_msg(Version, meter) ->
    of_msg_lib:get_meter_stats(Version, all).

process_reply(DatapathId, {ok, {flow_stats_reply, Body}}) ->
    Flows = proplists:get_value(flows, Body),
    {flows, process_flows(DatapathId, Flows)};
process_reply(DatapathId, {ok, {table_stats_reply, Body}}) ->
    Tables = proplists:get_value(tables, Body),
    {tables, process_tables(DatapathId, Tables)};
process_reply(DatapathId, {ok, {aggregate_stats_reply, Body}}) ->
    {aggregate, process_aggregates(DatapathId, Body)};
process_reply(DatapathId, {ok, {port_stats_reply, Body}}) ->
    Ports = proplists:get_value(ports, Body),
    {ports, process_ports(DatapathId, Ports)};
process_reply(DatapathId, {ok, {queue_stats_reply, Body}}) ->
    Queues = proplists:get_value(queues, Body),
    {queues, process_queues(DatapathId, Queues)};
process_reply(DatapathId, {ok, {group_stats_reply, Body}}) ->
    Groups = proplists:get_value(groups, Body),
    {groups, process_groups(DatapathId, Groups)};
process_reply(DatapathId, {ok, {meter_stats_reply, Body}}) ->
    Meters = proplists:get_value(meters, Body),
    {meters, process_meters(DatapathId, Meters)};
process_reply(DatapathId, {ok, {UnknownReply, _}}) ->
    ?INFO("Unknown stats poll reply: ~p(~p)~n", [DatapathId, UnknownReply]),
    [];
process_reply(DatapathId, {error, Reason}) ->
    ?ERROR("stats poll error: ~p(~p)~n", [DatapathId, Reason]),
    [].

% flow stats

process_flows(_DatapathId, undefined) ->
    [];
process_flows(DatapathId, Flows) ->
    lists:foldl(
        fun (Flow, Metrics) ->
            [make_stats(DatapathId, process_flow(Flow)) | Metrics]
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

process_tables(_DatapathId, undefined) ->
    [];
process_tables(DatapathId, Tables) ->
    lists:foldl(
        fun (Table, Metrics) ->
                [make_stats(DatapathId, process_table(Table)) | Metrics]
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

process_aggregates(DatapathId, Body) ->
    make_stats(DatapathId, process_aggregate(Body)).

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

process_ports(_DatapathId, undefined) ->
    [];
process_ports(DatapathId, Ports) ->
    lists:foldl(
        fun (Port, Metrics) ->
                [make_stats(DatapathId, process_port(Port)) | Metrics]
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

process_queues(_DatapathId, undefined) ->
    [];
process_queues(DatapathId, Queues) ->
    lists:foldl(
        fun(Queue, Metrics) ->
            [make_stats(DatapathId, process_queue(Queue)) | Metrics]
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

process_groups(_DatapathId, undefined) ->
    [];
process_groups(DatapathId, Groups) ->
    lists:foldl(
        fun (Group, Metrics) ->
            [make_stats(DatapathId, process_group(Group)) | Metrics]
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
    lists:foldl(
        fun (Bucket, {Count, Stats}) ->
                {Count + 1, [process_bucket(Count, Bucket) | Stats]}
        end, {0, []}, Buckets).

process_bucket(BucketId, Bucket) ->
    lists:foldl(
        fun ({packet_count, PCount}, Stat) ->
                Stat#bucket_stat{packet_count = PCount};
            ({byte_count, BCount}, Stat) ->
                Stat#bucket_stat{byte_count = BCount};
            (_, Stat) -> Stat
        end, #bucket_stat{bucket_id = BucketId}, Bucket).

% meter stats

process_meters(_DatapathId, undefined) ->
    [];
process_meters(DatapathId, Meters) ->
    lists:foldl(
        fun (Meter, Metrics) ->
            [make_stats(DatapathId, process_meter(Meter)) | Metrics]
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
    lists:foldl(
        fun (Band, {Count, Stats}) ->
                {Count + 1, [process_band(Count, Band) | Stats]}
        end, {0, []}, Bands).

process_band(BandId, Band) ->
    lists:foldl(
        fun ({packet_band_count, PCount}, Stat) ->
                Stat#band_stat{packet_band_count = PCount};
            ({byte_band_count, BCount}, Stat) ->
                Stat#band_stat{byte_band_count = BCount};
            (_, Stat) -> Stat
        end, #band_stat{band_id = BandId}, Band).

% folsom stats

make_stats(DatapathId, Stat) ->
    BaseName = folsom_basename(DatapathId, Stat),
    [{folsom_name(BaseName, Key), Value} || {Key, Value}
                                        <- folsom_stats(Stat)].

folsom_name(BaseName, Name) ->
    list_to_binary([BaseName, $_, Name]).

folsom_basename(DatapathId, #flow_stat{table_id = TableId,
                                              priority = Priority,
                                              cookie = Cookie}) ->
    [datapathid_to_binary(DatapathId),
     <<"-flow-">>,
     integer_to_binary(TableId),
     $-,
     binary_to_hex(Priority),
     $-,
     binary_to_hex(Cookie)];
folsom_basename(DatapathId, #table_stat{table_id = TableId}) ->
    [datapathid_to_binary(DatapathId),
     <<"-table-">>,
     integer_to_binary(TableId)];
folsom_basename(DatapathId, #aggregate_stat{}) ->
    [datapathid_to_binary(DatapathId),
     <<"-aggregate-">>];
folsom_basename(DatapathId, #port_stat{port_no = Port}) ->
    [datapathid_to_binary(DatapathId),
     <<"-port-">>, 
     integer_to_binary(Port)];
folsom_basename(DatapathId, #queue_stat{port_no = Port}) ->
    [datapathid_to_binary(DatapathId),
     <<"-queue-">>, 
     integer_to_binary(Port)];
folsom_basename(DatapathId, #group_stat{group_id = Group}) ->
    [datapathid_to_binary(DatapathId),
     <<"-group-">>, 
     integer_to_binary(Group)];
folsom_basename(DatapathId, #band_stat{band_id = Band}) ->
    [datapathid_to_binary(DatapathId),
     <<"-band-">>, 
     integer_to_binary(Band)].

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
    [{<<"packet_count">>, PCount},
     {<<"byte_count">>, BCount},
     {<<"ref_count">>, RCount},
     {<<"duration_sec">>, Duration},
     {<<"duration_nsec">>, NDuration},
     [folsom_stats(Bucket) || Bucket <- Buckets]];
folsom_stats(Stat = #bucket_stat{}) ->
    #bucket_stat{bucket_id = BucketId,
                 packet_count = PCount,
                 byte_count = BCount} = Stat,
    Base = [<<"bucket">>, $-, integer_to_list(BucketId), $-],
    [{[Base, <<"packet_count">>], PCount},
     {[Base, <<"byte_count">>], BCount}];
folsom_stats(Stat = #meter_stat{}) ->
    #meter_stat{flow_count = FCount,
                packet_in_count = PCount,
                byte_in_count = BCount,
                duration_sec = Duration,
                duration_nsec = NDuration,
                band_stats = Bands} = Stat,
    [{<<"flow_count">>, FCount},
     {<<"byte_in_count">>, BCount},
     {<<"packet_in_count">>, PCount},
     {<<"duration_sec">>, Duration},
     {<<"duration_nsec">>, NDuration},
     [folsom_stats(Band) || Band <- Bands]];
folsom_stats(Stat = #band_stat{}) ->
    #band_stat{band_id = BandId,
               packet_band_count = PCount,
               byte_band_count = BCount} = Stat,
    Base = [<<"band">>, $-, integer_to_list(BandId), $-],
    [{[Base, <<"packet_band_count">>], PCount},
     {[Base, <<"byte_band_count">>], BCount}].

datapathid_to_binary({I, B}) ->
    [integer_to_binary(I), $:, binary_to_hex(B)].

binary_to_hex(Bin) when is_binary(Bin) ->
    [byte_to_hex(Byte) || <<Byte>> <- Bin].

byte_to_hex(<<N1:4, N2:4>>) ->
    [erlang:integer_to_list(N1, 16), erlang:integer_to_list(N2, 16)].
