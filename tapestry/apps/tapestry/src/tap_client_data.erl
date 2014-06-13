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
         nci/3,
         qps/3,
         new_client/1,
         more_nci_data/4,
         nci_details/1,
         collectors/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("tap_logger.hrl").

-define(STATE, tap_client_data_state).
-record(?STATE,{start_time,
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
                communities = no_communities}).

%------------------------------------------------------------------------------
% API
%------------------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

num_endpoints(Endpoints, Edges, DateTime) ->
    gen_server:cast(?MODULE, {num_endpoints, Endpoints, Edges, DateTime}).

nci(NCI, Communities, DateTime) ->
    gen_server:cast(?MODULE, {nci, NCI, Communities, DateTime}).

qps(QPS, Collectors, DateTime) ->
    gen_server:cast(?MODULE, {qps, QPS, Collectors, DateTime}).

new_client(Pid) ->
    gen_server:cast(?MODULE, {new_client, Pid}).

more_nci_data(Pid, Start, End, Max) ->
    gen_server:cast(?MODULE, {more_nci_data, Pid, Start, End, Max}).

nci_details(Pid) ->
    gen_server:cast(?MODULE, {nci_details, Pid}).

collectors(Pid) ->
    gen_server:cast(?MODULE, {collectors, Pid}).

%------------------------------------------------------------------------------
% gen_server callbacks
%------------------------------------------------------------------------------

init([])->
    gen_server:cast(?MODULE, start),
    {ok, #?STATE{}}.

% for debugging
handle_call(nci_details, _From, State = #?STATE{communities = Communities,
                                                nci = NCI,
                                                last_nci_time = Time}) ->
    {reply, json_nci_details(Time, NCI, Communities), State};
handle_call(collectors, _From, State = #?STATE{collectors = Collectors,
                                               last_qps_time = Time}) ->
    {reply, json_collectors(Time, Collectors), State};
handle_call(Msg, From, State) ->
    error({no_handle_call, ?MODULE}, [Msg, From, State]).

handle_cast(start, State) ->
    StartTime = calendar:universal_time(),
    Time = list_to_binary(tap_time:rfc3339(StartTime)),
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
            Time = list_to_binary(tap_time:rfc3339(UT)),
            JSON = encode_nep(Time, NEP, NE),
            broadcast_msg(Clients, JSON),
            State#?STATE{last_nep = JSON, last_int_nep = NEP};
        false -> State
    end,
    {noreply, NewState};
handle_cast({nci, NCI, Communities, UT}, State = #?STATE{nci_log = NCILog,
                                                         clients = Clients}) ->
    true = ets:insert(NCILog, {UT, NCI}),
    Time = list_to_binary(tap_time:rfc3339(UT)),
    JSON = encode_nci(Time, NCI),
    broadcast_msg(Clients, JSON),
    {noreply, State#?STATE{last_nci = JSON,
                           nci = NCI,
                           last_nci_time = Time,
                           communities = Communities}};
handle_cast({nci_details, Pid}, State = #?STATE{
                                            communities = Communities,
                                            nci = NCI,
                                            last_nci_time = Time}) ->
    clientsock:send(Pid, json_nci_details(Time, NCI, Communities)),
    {noreply, State};
handle_cast({collectors, Pid}, State = #?STATE{collectors = Collectors,
                                               last_qps_time = Time}) ->
    clientsock:send(Pid, json_collectors(Time, Collectors)),
    {noreply, State};
handle_cast({qps, QPS, Collectors, UT}, State = #?STATE{clients = Clients}) ->
    Time = list_to_binary(tap_time:rfc3339(UT)),
    QPSMsg = encode_qps(Time, QPS),
    broadcast_msg(Clients, QPSMsg),
    COLMsg = encode_cols(Time, length(Collectors)),
    broadcast_msg(Clients, COLMsg),
    {noreply, State#?STATE{last_qps = QPSMsg,
                           last_col = COLMsg,
                           last_qps_time = Time,
                           collectors = Collectors}};
handle_cast({new_client, Pid}, State = #?STATE{clients = Clients,
                                               start_time = StartTime,
                                               last_nci = LNCI,
                                               last_nep = LNEP,
                                               last_qps = LQPS,
                                               last_col = COLS}) ->
    monitor(process, Pid),
    ?DEBUG("tap_client_data: new client ~p~n",[Pid]),
    HELLO = jiffy:encode({[{<<"action">>, <<"hello">>},
                           {<<"start_time">>,
                                list_to_binary(tap_time:rfc3339(StartTime))},
                           {<<"current_time">>,
                                list_to_binary(tap_time:rfc3339(calendar:universal_time()))}]}),
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

send_more_data(Pid, Data) when is_pid(Pid), is_list(Data)->
    ?DEBUG("tap_client_data: sending more data ~p to ~p~n",[Data,Pid]),
    JSONData = lists:foldl(
                    fun({Time, Value}, AccIn) ->
                        [{[{<<"Time">>, list_to_binary(tap_time:rfc3339(Time))},
                         {<<"NCI">>, Value}]} | AccIn]
                    end, [], Data),
    JSON = jiffy:encode(JSONData),
    clientsock:send(Pid, JSON).

broadcast_msg(Clients, Msg) ->
    [clientsock:send(C, Msg) || C <- Clients].

json_nci_details(Time, undefined, Communities) ->
    json_nci_details(Time, 0, Communities);
json_nci_details(Time, NCI, Communities) ->
    jiffy:encode({[
        {<<"action">>,<<"NCIDetails">>},
        {<<"NCI">>,NCI},
        {<<"Time">>, Time},
        {<<"Communities">>, community_details(Communities)},
        {<<"CommunityGraph">>, community_graph(Communities)}
    ]}).

community_graph(no_communities) ->
    [];
community_graph({_Endpoints, _Interactions, Sizes,
                                        {Cendpoints, Cinteractions}}) ->
    {[
        {<<"Endpoints">>, cendpoints(Cendpoints, Sizes)},
        {<<"Interactions">>, cinteractions(Cinteractions, Sizes)}
    ]}.

cendpointsize(C, Sizes) ->
    Size = integer_to_binary(dict:fetch(C, Sizes)),
    <<C/binary, $:, Size/binary>>.

cendpoints(Endpoints, Sizes) ->
    lists:map(fun(E) -> cendpointsize(E, Sizes) end, Endpoints).

cinteractions(Interactions, Sizes) ->
    [[cendpointsize(C1, Sizes),
      cendpointsize(C2, Sizes)] || {C1,C2} <- Interactions].

community_details(no_communities) ->
    [];
community_details({Endpoints, Interactions, Sizes, _Comms}) ->
    [
        {[
            {<<"Interactions">>, interactions(dict_fetch(C, Interactions))},
            {<<"Endpoints">>, endpoints(dict_fetch(C, Endpoints))},
            {<<"Size">>, dict:fetch(C, Sizes)}
        ]} || C <- dict:fetch_keys(Sizes)
    ].

dict_fetch(Key, Dict) ->
    case dict:find(Key, Dict) of
        {ok, V} -> V;
        error -> []
    end.

interactions(L) ->
    Interactions = lists:foldl(
        fun(I, S) ->
            NI = normalize_interaction(I),
            sets:add_element(NI, S)
        end, sets:new(), L),
    [[endpoint(A), endpoint(B)] || {A, B} <- sets:to_list(Interactions)].

normalize_interaction({A, B}) when A > B ->
    {A, B};
normalize_interaction({A, B}) ->
    {B, A}.

endpoints(L) ->
    [endpoint(E) || E <- L].

endpoint({A,B,C,D}) ->
    list_to_binary([integer_to_list(A), ".", integer_to_list(B), ".", 
                                integer_to_list(C), ".", integer_to_list(D)]);
endpoint(B) when is_binary(B) ->
    B;
endpoint(S) when is_list(S) ->
    list_to_binary(S);
endpoint(_) ->
    <<"invalid">>.

json_collectors(Time, Collectors) ->
    jiffy:encode({[
        {<<"action">>,<<"collectors">>},
        {<<"Time">>, Time},
        {<<"Collectors">>, format_collectors(Collectors)}
    ]}).

format_collectors(Collectors) ->
    {JSON, _} = lists:mapfoldl(
        fun(C, Index) ->
            Name = list_to_binary(["Collector", integer_to_list(Index)]),
            {{[{<<"name">>,Name} | collector(C)]}, Index + 1}
        end, 0, Collectors),
    JSON.

collector({ofswitch, DatapathId, IpAddr, QPS}) ->
    [
        {<<"collector_type">>,<<"OF1.3 Switch">>},
        {<<"ip">>,endpoint(IpAddr)},
        {<<"datapath_id">>,list_to_binary(DatapathId)},
        {<<"qps">>,format_qps(QPS)}
    ];
collector({grid, IpAddr, QPS}) ->
    [
        {<<"collector_type">>,<<"IB Grid Member">>},
        {<<"ip">>,endpoint(IpAddr)},
        {<<"datapath_id">>,<<>>},
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
