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
%% @doc
%% Tap client data.  Hub of the system.  Receives data from the NCI
%% calculator and pushes the data to the clients.

-module(tap_client_data).

-behavior(gen_server).

-export([start_link/0,
         num_endpoints/3,
         nci/6,
         qps/4,
         new_client/1,
         more_nci_data/4,
         nci_details/1,
         limits/1,
         setlimits/2,
         collectors/1,
         getconfig/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("tap_logger.hrl").

-define(STATE, tap_client_data_state).
-record(?STATE,{start_time,
                limits,
                clients = [],
                last_nci,
                last_nci_time,
                nci_log,
                last_nep,
                last_int_nep = 0,
                last_qps,
                last_col,
                last_qps_time,
                nci,
                collectors = [],
                use_graphviz,
                neato_bin,
                community_data = no_communities}).

-define(DOTFILENAME, "/tmp/tapestry.dot").
-define(NEATO_OPTS, "-Tplain").
-define(CONFIGVARS, [
    {tapestry, config_file},
    {tapestry, web_address},
    {tapestry, web_port},
    {tapestry, web_log},
    {tapestry, web_id},
    {tapestry, ftpd_address},
    {tapestry, ftpd_port},
    {tapestry, datasources},
    {tapestry, max_collector_idle_time},
    {tapestry, nci_min_interval},
    {tapestry, max_vertices},
    {tapestry, max_edges},
    {tapestry, max_communities},
    {tapestry, comm_size_limit},
    {tapestry, qps_max_interval},
    {tapestry, clean_interval},
    {tapestry, data_max_age},
    {tapestry, use_graphviz},
    {tapestry, neato_bin},
    {tapestry, community_detector},
    {tapestry, requester_whitelist},
    {tapestry, requester_blacklist},
    {tapestry, resolved_whitelist},
    {tapestry, resolved_blacklist},
    {tapestry, query_whitelist},
    {tapestry, query_blacklist},
    {tapestry, save_files},
    {of_driver, listen_ip},
    {of_driver, listen_port}
]).

-record(community_data, {
    communities,
    sizes,
    cgraph,
    vertexinfo          % dictionary[Vertex] = Label
}).

%------------------------------------------------------------------------------
% API
%------------------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

num_endpoints(Endpoints, Edges, DateTime) ->
    gen_server:cast(?MODULE, {num_endpoints, Endpoints, Edges, DateTime}).

nci(NCI, Communities, CommunitySizes, CommunityGraph, VertexInfo, DateTime) ->
    CommunityData = #community_data{
        communities = Communities,
        sizes = CommunitySizes,
        cgraph = CommunityGraph,
        vertexinfo = VertexInfo
    },
    gen_server:cast(?MODULE, {nci, NCI, CommunityData, DateTime}).

qps(Sender, QPS, Collectors, DateTime) ->
    gen_server:cast(?MODULE, {qps, Sender, QPS, Collectors, DateTime}).

new_client(Pid) ->
    gen_server:cast(?MODULE, {new_client, Pid}).

more_nci_data(Pid, Start, End, Max) ->
    gen_server:cast(?MODULE, {more_nci_data, Pid, Start, End, Max}).

nci_details(Pid) ->
    gen_server:cast(?MODULE, {nci_details, Pid}).

limits(Pid) ->
    gen_server:cast(?MODULE, {limits, Pid}).

setlimits(Pid, Limits) ->
    gen_server:cast(?MODULE, {setlimits, Pid, Limits}).

collectors(Pid) ->
    gen_server:cast(?MODULE, {collectors, Pid}).

getconfig(Pid) ->
    gen_server:cast(?MODULE, {getconfig, Pid}).

%------------------------------------------------------------------------------
% gen_server callbacks
%------------------------------------------------------------------------------

init([]) ->
    gen_server:cast(?MODULE, start),
    Limits = [{Key, tap_config:getconfig(Key)} || Key <-
                [max_vertices, max_edges, max_communities, comm_size_limit]],
    UseGraphViz = tap_config:getconfig(use_graphviz),
    NeatoBin = tap_config:getconfig(neato_bin),
    {ok, #?STATE{neato_bin = NeatoBin,
                 use_graphviz = UseGraphViz,
                 collectors = dict:new(),
                 limits = Limits}}.

% for debugging
handle_call(nci_details, _From, State = #?STATE{community_data = CommunityData,
                                                nci = NCI,
                                                last_nci_time = Time,
                                                neato_bin = NeatoBin,
                                                use_graphviz = UseGraphViz,
                                                limits = Limits}) ->
    Neato = neato_cmd(NeatoBin),
    {reply,
        json_nci_details(Time, NCI, CommunityData, Limits, Neato, UseGraphViz),
        State};
handle_call(dot_community_details, _From,
                        State = #?STATE{community_data = CommunityData}) ->
    {reply, dot_community_details(CommunityData), State};
handle_call(collectors, _From, State = #?STATE{collectors = CollectorDict,
                                               last_qps_time = Time}) ->
    {reply, json_collectors(Time, CollectorDict), State};
handle_call(getconfig, _From, State = #?STATE{}) ->
    {reply, json_config(), State};
handle_call(Msg, From, State) ->
    error({no_handle_call, ?MODULE}, [Msg, From, State]).

handle_cast(start, State) ->
    StartTime = calendar:universal_time(),
    Time = rfc3339bin(StartTime),
    COLS = encode_cols(Time, 0),
    LNCI = encode_nci(Time, 0),
    LNEP = encode_nep(Time, 0, 0),
    LQPS = encode_qps(Time, 0),
    {noreply, State#?STATE{start_time = StartTime,
                      last_nci = LNCI,
                      nci_log = ets:new(nci_log, [ordered_set]),
                      last_nep = LNEP,
                      last_int_nep = 0,
                      last_qps = LQPS,
                      last_col = COLS}};
handle_cast({num_endpoints, NEP, NE, UT},
                                    State = #?STATE{last_int_nep = LIntNEP,
                                                    clients = Clients}) ->
    NewState = case NEP =/= LIntNEP of
        true ->
            Time = rfc3339bin(UT),
            JSON = encode_nep(Time, NEP, NE),
            broadcast_msg(Clients, JSON),
            State#?STATE{last_nep = JSON, last_int_nep = NEP};
        false -> State
    end,
    {noreply, NewState};
handle_cast({nci, NCI, CommunityData, UT}, State = #?STATE{nci_log = NCILog,
                                                        clients = Clients}) ->
    true = ets:insert(NCILog, {UT, NCI}),
    Time = rfc3339bin(UT),
    JSON = encode_nci(Time, NCI),
    broadcast_msg(Clients, JSON),
    {noreply, State#?STATE{last_nci = JSON,
                           nci = NCI,
                           last_nci_time = Time,
                           community_data = CommunityData
                          }};
handle_cast({limits, Pid}, State = #?STATE{limits = Limits}) ->
    clientsock:send(Pid, json_limits(<<"getlimits">>, Limits)),
    {noreply, State};
handle_cast({getconfig, Pid}, State = #?STATE{}) ->
    clientsock:send(Pid, json_config()),
    {noreply, State};
handle_cast({setlimits, Pid, SetLimits}, State = #?STATE{limits = Limits}) ->
    NewLimits = do_setlimits(Limits, SetLimits),
    clientsock:send(Pid, json_limits(<<"setlimits">>, NewLimits)),
    {noreply, State#?STATE{limits = NewLimits}};
handle_cast({nci_details, Pid}, State = #?STATE{
                                            community_data = CommunityData,
                                            nci = NCI,
                                            last_nci_time = Time,
                                            neato_bin = NeatoBin,
                                            use_graphviz = UseGraphViz,
                                            limits = Limits}) ->
    Neato = neato_cmd(NeatoBin),
    clientsock:send(Pid,
        json_nci_details(Time, NCI, CommunityData, Limits, Neato, UseGraphViz)),
    {noreply, State};
handle_cast({collectors, Pid}, State = #?STATE{collectors = CollectorDict,
                                               last_qps_time = Time}) ->
    clientsock:send(Pid, json_collectors(Time, CollectorDict)),
    {noreply, State};
handle_cast({qps, Sender, QPS, Collectors, UT},
                                State = #?STATE{clients = Clients,
                                                collectors = CollectorDict}) ->
    NewCollectorDict = save_collector(Sender, QPS, Collectors, CollectorDict),
    Time = rfc3339bin(UT),
    QPSMsg = encode_qps(Time, collector_qps(NewCollectorDict)),
    broadcast_msg(Clients, QPSMsg),
    COLMsg = encode_cols(Time, collector_count(NewCollectorDict)),
    broadcast_msg(Clients, COLMsg),
    {noreply, State#?STATE{last_qps = QPSMsg,
                           last_col = COLMsg,
                           last_qps_time = Time,
                           collectors = NewCollectorDict}};
handle_cast({new_client, Pid}, State = #?STATE{clients = Clients,
                                               start_time = StartTime,
                                               last_nci = LNCI,
                                               last_nep = LNEP,
                                               last_qps = LQPS,
                                               last_col = COLS}) ->
    monitor(process, Pid),
    ?DEBUG("tap_client_data: new client ~p~n",[Pid]),
    HELLO = jiffy:encode({[{<<"action">>, <<"hello">>},
                           {<<"start_time">>, rfc3339bin(StartTime)},
                           {<<"current_time">>,
                                rfc3339bin(calendar:universal_time())}]}),
    clientsock:send(Pid, HELLO),
    clientsock:send(Pid, LNCI),
    clientsock:send(Pid, LNEP),
    clientsock:send(Pid, LQPS),
    clientsock:send(Pid, COLS),
    {noreply, State#?STATE{clients = [Pid | Clients]}};
handle_cast({more_nci_data, Pid, Start, End, MaxData},
                                        State = #?STATE{nci_log = NCILog}) ->
    ?DEBUG("tap_client_data: {more_nci_data,~p,~p,~p,~p}~n",[Pid,Start,End,MaxData]),
    spawn(
        fun() ->
            Target = ets:foldl(
                        fun(X, AccIn) ->
                            {Time, _Value} = X,
                            case (Time >= Start) and (Time =< End) of
                                true -> [X | AccIn];
                                false -> AccIn
                            end
                        end, [], NCILog),
            case length(Target) > 0 of
                true ->
                    send_more_data(Pid, tap_data:sample_down(Target, MaxData));
                false ->
                    ?DEBUG("tap_client_data: NO RESULTS (i.e. empty set) for {more_nci_data,...} request from ~p~n",[Pid])
            end
        end),
{noreply, State};
handle_cast(Msg, State) ->
    error({no_handle_cast, ?MODULE}, [Msg, State]).

handle_info({'DOWN', _, process, Pid, _}, State = #?STATE{clients = Clients}) ->
    ?DEBUG("client down ~p~n", [Pid]),
    {noreply, State#?STATE{clients = lists:delete(Pid, Clients)}};
handle_info(Msg, State) ->
    error({no_handle_info, ?MODULE}, [Msg, State]).

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

%------------------------------------------------------------------------------
% local functions
%------------------------------------------------------------------------------

neato_cmd(NeatoBin) ->
    [NeatoBin, " ", ?NEATO_OPTS].

save_collector(Sender, QPS, Collectors, CollectorDict) ->
    dict:store(Sender, {QPS, Collectors}, CollectorDict).

collector_count(CollectorDict) ->
    dict:fold(fun(_, {_, Collectors}, Count) ->
                  Count + length(Collectors)
              end, 0, CollectorDict).

collector_qps(CollectorDict) ->
    dict:fold(fun(_, {QPS, _}, TotalQPS) ->
                  TotalQPS + QPS
              end, 0, CollectorDict).

send_more_data(Pid, Data) when is_pid(Pid), is_list(Data)->
    ?DEBUG("tap_client_data: sending more data ~p to ~p~n",[Data,Pid]),
    JSONData = lists:foldl(
                    fun({Time, Value}, AccIn) ->
                        [{[{<<"Time">>, rfc3339bin(Time)},
                         {<<"NCI">>, Value}]} | AccIn]
                    end, [], Data),
    JSON = jiffy:encode(JSONData),
    clientsock:send(Pid, JSON).

broadcast_msg(Clients, Msg) ->
    [clientsock:send(C, Msg) || C <- Clients].

json_nci_details(_Time, undefined, no_communities, _Limits, _Neato, _UseGraphViz) ->
    <<>>;
json_nci_details(Time, undefined, CommunityData, Limits, Neato, UseGraphViz) ->
    json_nci_details(Time, 0, CommunityData, Limits, Neato, UseGraphViz);
json_nci_details(Time, NCI,
            CommunityData = #community_data{sizes = Sizes,
                                            vertexinfo = VertexInfoD},
            Limits, Neato, UseGraphViz) ->
    MaxCommunities = proplists:get_value(max_communities, Limits),
    % make a list of the largest communities and truncate the list after
    % MaxCommunities elements.
    CommunityList = [C || {C, _} <- lists:sublist(lists:sort(
                                        fun({_, A}, {_, B}) ->
                                            A > B % reverse sort
                                    end, dict:to_list(Sizes)), MaxCommunities)],
    jiffy:encode({[
        {<<"action">>,<<"NCIDetails">>},
        {<<"NCI">>,NCI},
        {<<"Time">>, Time},
        {<<"Communities">>, community_details(CommunityData,
                                    CommunityList, Limits, Neato, UseGraphViz)},
        {<<"CommunityGraph">>, community_graph(CommunityData,
                                                CommunityList, Limits)},
        {<<"Labels">>, labels(VertexInfoD)},
        {<<"Requesters">>, requesters(VertexInfoD)}
    ]}).

requesters(VertexInfoD) ->
    dict:fold(
        fun(K, {_, PL}, L) ->
            case is_requester(PL) of
                true -> [endpoint(K) | L];
                false -> L
            end
        end, [], VertexInfoD).

is_requester(PL) ->
    proplists:get_value(who, PL) == requester.

labels(VertexInfoD) ->
    {
        [{endpoint(K), label(PL)} || {K,{_, PL}} <- dict:to_list(VertexInfoD)]
    }.

label(PL) ->
    format_label(name_value(proplists:get_value(label, PL))).

format_label(Name) ->
    {[{<<"Name">>, Name}]}.

name_value(undefined) ->
    <<"undefined">>;
name_value(L) when is_binary(L) ->
    L;
name_value(L) when is_list(L) ->
    list_to_binary(L).

community_graph(no_communities, _CommunityList, _Limits) ->
    [];
community_graph(#community_data{sizes = Sizes,
                                cgraph = {_Cendpoints, Cinteractions}
                               },
                               CommunityList, _Limits) ->
    {[
        {<<"Endpoints">>, cendpoints(CommunityList, Sizes)},
        {<<"Interactions">>, cinteractions(Cinteractions, CommunityList, Sizes)}
    ]}.

cendpointsize(C, Sizes) ->
    FormattedC = endpoint(C),
    SizeI = dict:fetch(C, Sizes),
    Size = integer_to_binary(SizeI),
    <<FormattedC/binary, $|, Size/binary>>.

cendpoints(Endpoints, Sizes) ->
    lists:map(fun(E) -> cendpointsize(E, Sizes) end, Endpoints).

cinteractions(Interactions, CommunityList, Sizes) ->
    CommunitySet = sets:from_list(CommunityList),
    lists:foldl(
        fun({C1, C2}, L) ->
            case sets:is_element(C1, CommunitySet) andalso
                 sets:is_element(C2, CommunitySet) of
                true ->
                    [[cendpointsize(C1, Sizes),
                     cendpointsize(C2, Sizes)] | L];
                false ->
                    L
            end
        end, [], Interactions).

% dict_value_length(D) ->
%     dict:fold(fun(_, List, Sum) -> Sum + length(List) end, 0, D).

community_details(no_communities, _CommunityList, _Limits, _Neato, _UseGraphViz) ->
    [];
community_details(#community_data{communities = {EndpointsD, InteractionsD},
                                  sizes = SizesD
                                 },
                                 CommunityList, Limits, Neato, UseGraphViz) ->
    MaxVertices = proplists:get_value(max_vertices, Limits),
    MaxEdges = proplists:get_value(max_edges, Limits),
    lists:foldl(
        fun(C, L) ->
            Interactions = dict_fetch(C, InteractionsD),
            Endpoints = dict_fetch(C, EndpointsD),
            D = case overlimit(length(Interactions), MaxEdges) orelse
                        overlimit(length(Endpoints), MaxVertices) of
                true ->
                    {[
                        {<<"Interactions">>, []},
                        {<<"Endpoints">>, []},
                        {<<"Label">>, endpoint(C)},
                        {<<"Size">>, dict:fetch(C, SizesD)}
                    ]};
                false ->
                    Body = [
                        {<<"Interactions">>, interactions(Interactions)},
                        {<<"Endpoints">>, endpoints(Endpoints)},
                        {<<"Label">>, endpoint(C)},
                        {<<"Size">>, dict:fetch(C, SizesD)}
                    ] ++ gz_community_details(Neato, Endpoints, Interactions, UseGraphViz),
                    {Body}
            end,
            [D | L]
        end, [], CommunityList).

gz_community_details(Neato, Endpoints, Interactions, true) ->
    {Width, Height, Nodes} = gz_endpoints(Neato, Endpoints, Interactions),
    [
        {<<"Sizes">>, {[
            {<<"height">>, Height},
            {<<"width">>, Width}
        ]}},
        {<<"GEndpoints">>, format_gz_nodes(Nodes)}
    ];
gz_community_details(_Neato, _Endpoints, _Interactions, false) ->
    [].

overlimit(_, 0) ->
    false;
overlimit(_, inifinity) ->
    false;
overlimit(Value, Max) ->
    Value > Max.

dict_fetch(Key, Dict) ->
    case dict:find(Key, Dict) of
        {ok, V} -> V;
        error -> []
    end.

interactions(Interactions) ->
    [[endpoint(A), endpoint(B)] || {A, B} <- Interactions].

endpoints(L) ->
    [endpoint(E) || E <- L].

endpoint(A = {_,_,_,_}) ->
    list_to_binary(inet:ntoa(A));
endpoint(A = {_,_,_,_,_,_,_,_}) ->
    list_to_binary(inet:ntoa(A));
endpoint(B) when is_binary(B) ->
    B;
endpoint(S) when is_list(S) ->
    list_to_binary(S);
endpoint(_) ->
    <<"invalid">>.

gz_endpoints(Neato, Endpoints, Edges) ->
    DotFile = dot_community_graph(Endpoints, Edges),
    graphviz(Neato, DotFile).


% [[Vertex, X, Y]]
format_gz_nodes(Nodes) ->
    lists:map(
        fun([Vertex, X, Y]) ->
            {[
                {<<"Id">>, Vertex},
                {<<"x">>, binary_to_number(X)},
                {<<"y">>, binary_to_number(Y)}
            ]}
        end, Nodes).

json_collectors(Time, CollectorDict) ->
    jiffy:encode({[
        {<<"action">>,<<"collectors">>},
        {<<"Time">>, Time},
        {<<"Collectors">>, format_collectors(CollectorDict)}
    ]}).

format_collectors(CollectorDict) ->
    Collectors = lists:append(
                    dict:fold(
                        fun(_, {_, Cols}, L) ->
                            [Cols | L]
                        end, [], CollectorDict)),
    {JSON, _} = lists:mapfoldl(
        fun(C, Index) ->
            Name = list_to_binary(["Collector", integer_to_list(Index)]),
            {{[{<<"name">>,Name} | collector(C)]}, Index + 1}
        end, 0, Collectors),
    JSON.

collector({ofswitch, DatapathId, IpAddr, LastUpdate, QPS}) ->
    [
        {<<"collector_type">>,<<"OF1.3 Switch">>},
        {<<"ip">>,endpoint(IpAddr)},
        {<<"datapath_id">>,list_to_binary(DatapathId)},
        {<<"lastupdate">>,rfc3339bin(LastUpdate)},
        {<<"qps">>,format_qps(QPS)}
    ];
collector({grid, IpAddr, LastUpdate, QPS}) ->
    [
        {<<"collector_type">>,<<"IB Grid Member">>},
        {<<"ip">>,endpoint(IpAddr)},
        {<<"datapath_id">>,<<>>},
        {<<"lastupdate">>,rfc3339bin(LastUpdate)},
        {<<"qps">>,format_qps(QPS)}
    ].

format_qps(N) ->
    float_to_binary(float(N), [{decimals,4}, compact]).

encode_cols(Time, Cols) ->
    jiffy:encode({[{<<"action">>, <<"Collectors">>},
                   {<<"Time">>, Time},
                   {<<"COLLECTORS">>, Cols}]}).

encode_nci(Time, Nci) ->
    jiffy:encode({[{<<"action">>, <<"NCI">>},
                   {<<"Time">>, Time},
                   {<<"NCI">>, Nci}]}).

encode_nep(Time, Nep, NE) ->
    jiffy:encode({[{<<"action">>, <<"NEP">>},
                   {<<"Time">>, Time},
                   {<<"NE">>, NE},
                   {<<"NEP">>, Nep}]}).

encode_qps(Time, QPS) ->
    jiffy:encode({[{<<"action">>, <<"QPS">>},
                   {<<"Time">>, Time},
                   {<<"QPS">>, QPS}]}).

json_config() ->
    jiffy:encode({[{<<"action">>, <<"getconfig">>},
                   {<<"config">>, {
                        [encode_config(get_config(C)) || C <- ?CONFIGVARS]
                                  }}
                  ]}).

get_config({Application, Key}) ->
    case application:get_env(Application, Key) of
        undefined -> {Key, undefined};
        {ok, Value} -> {Key, Value}
    end.

encode_config({K, V}) ->
    {atom_to_binary(K, utf8), iolist_to_binary(io_lib:format("~p",[V]))}.

json_limits(Action, PList) ->
    jiffy:encode(
        {[{<<"action">>, Action},
          {<<"limits">>, {[
                            {atom_to_binary(K, latin1), encode_limit(V)} ||
                                {K, V} <- PList
                         ]}
          }
        ]}
    ).

encode_limit(infinity) ->
    <<"infinity">>;
encode_limit(I) when is_integer(I) ->
    I.

do_setlimits(Limits0, NewLimits) ->
    lists:foldl(
        fun({Limit, Value}, Limits) ->
            update_limit(Limits, Limit, decode_limit(Value))
        end, Limits0, NewLimits).

decode_limit(<<"infinity">>) -> infinity;
decode_limit(I) when is_integer(I) -> I.

update_limit(Limits, Limit, Value) ->
    lists:keyreplace(Limit, 1, Limits, {Limit, Value}).

% output:
% {Width, Height, [[Vertex, X, Y]]}
% Values are binaries
graphviz(Neato, DotFile) ->
    % delete old file
    file_delete(?DOTFILENAME),

    % write out file
    ok = file:write_file(?DOTFILENAME, DotFile),

    % run neato and collect output
    Plain = os:cmd([Neato, " ", ?DOTFILENAME]),

    % parse for output
    parse_plain(Plain).

% plain graph data format:
% graph 1 1.0529 0.91677
% node "1.2.3.4" 0.025 0.88519 0.05 0.05 "1.2.3.4" solid point black lightgrey
% node "2.3.4.5" 1.0279 0.89177 0.05 0.05 "2.3.4.5" solid point black lightgrey
% node "3.4.5.6" 0.53322 0.025 0.05 0.05 "3.4.5.6" solid point black lightgrey
% edge "1.2.3.4" "2.3.4.5" 4 0.053932 0.88538 0.20038 0.88634 0.8585 0.89066 1.0007 0.89159 solid black
% edge "2.3.4.5" "3.4.5.6" 4 1.0136 0.86677 0.94136 0.74019 0.61676 0.17138 0.54661 0.048465 solid black
% edge "3.4.5.6" "1.2.3.4" 4 0.51856 0.049816 0.44435 0.17543 0.11083 0.73993 0.038758 0.86191 solid black
% stop
%
% output:
% {Width, Height, [[Vertex, X, Y]]}
% Values are binaries
parse_plain(Plain) ->
    {match, [Width, Height]} = re:run(Plain, "^graph [0-9]+ ([.0-9]+) ([.0-9]+)", [{capture, all_but_first, binary}]),
    {match, Nodes} = re:run(Plain, "^node \"([^\"]+)\" ([.0-9]+) ([.0-9]+)", [global, multiline, {capture, all_but_first, binary}]),
    {binary_to_number(Width), binary_to_number(Height), Nodes}.

dot_community_details(no_communities) ->
    [];
dot_community_details({{EndpointsD, InteractionsD}, _SizesD, _CommGraph}) ->
    % list of unique endpoints
    NodesS = set_from_dict_values_list(EndpointsD),
    % unique edges
    EdgesS = set_from_dict_values_list(InteractionsD),
    dot_community_graph(sets:to_list(NodesS), sets:to_list(EdgesS), EndpointsD).

dot_community_graph(Nodes, Edges) ->
    dot_community_graph(Nodes, Edges, no_subgraph).

dot_community_graph(Nodes, Edges, EndpointsD) ->
    [
        <<"graph {\n\n">>,
        format_nodes(Nodes), <<"\n">>,
        format_edges(Edges), <<"\n">>,
        format_subgraphs(EndpointsD), <<"\n">>,
        <<"}\n">>
    ].

set_from_dict_values_list(Dict) ->
    dict:fold(
        fun(_, ValueList, Set) ->
            sets:union([Set, sets:from_list(ValueList)])
        end, sets:new(), Dict).

format_nodes(Nodes) ->
    format_list(
        fun(Node) ->
            [<<"node[shape=point] ">>,$",endpoint(Node),$",<<"\n">>]
        end, Nodes).

format_edges(Edges) ->
    format_list(
        fun({N1, N2}) ->
            [$",endpoint(N1),$"," -- ",$",endpoint(N2),$",<<"\n">>]
        end, Edges).

format_subgraphs(no_subgraph) ->
    [];
format_subgraphs(EndpointsD) ->
    dict:fold(
        fun(_, Endpoints, IOList) ->
            [format_subgraph(Endpoints) | IOList]
        end, [], EndpointsD).

format_subgraph(Endpoints) ->
    [<<"subgraph { ">>, join_endpoints(Endpoints), $}, <<"\n">>].

join_endpoints([]) ->
    [];
join_endpoints([Endpoint | Endpoints]) ->
    join_endpoints(Endpoints, [format_endpoint(Endpoint)]).

join_endpoints([], IOList) ->
    IOList;
join_endpoints([Endpoint | RestEndpoints], IOList) ->
    join_endpoints(RestEndpoints,
                            [format_endpoint(Endpoint), <<", ">> | IOList]).

format_endpoint(Endpoint) ->
    [$", endpoint(Endpoint), $"].

format_list(FormatFn, List) ->
        lists:foldl(
            fun(Element, IOList) ->
                [FormatFn(Element) | IOList]
            end, [], List).

file_delete(Filename) ->
    % XXX log it not enoent?
    file:delete(Filename).

binary_to_number(B) when is_binary(B) ->
    try binary_to_float(B)
    catch
        error:badarg ->
            binary_to_integer(B)
    end.

rfc3339bin(T) ->
    list_to_binary(tap_time:rfc3339(T)).
