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
         num_endpoints/2,
         nci/3,
         qps/2,
         new_client/1,
         more_nci_data/4,
         nci_details/1]).

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
                nci_log,
                last_nep,
                last_int_nep = 0,
                last_qps,
                nci,
                communities = {dict:new(), dict:new()}}).

%------------------------------------------------------------------------------
% API
%------------------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

num_endpoints(Endpoints, DateTime) ->
    gen_server:cast(?MODULE, {num_endpoints, Endpoints, DateTime}).

nci(NCI, Communities, DateTime) ->
    gen_server:cast(?MODULE, {nci, NCI, Communities, DateTime}).

qps(QPS, DateTime) ->
    gen_server:cast(?MODULE, {qps, QPS, DateTime}).

new_client(Pid) ->
    gen_server:cast(?MODULE, {new_client, Pid}).

more_nci_data(Pid, Start, End, Max) ->
    gen_server:cast(?MODULE, {more_nci_data, Pid, Start, End, Max}).

nci_details(Pid) ->
    gen_server:cast(?MODULE, {nci_details, Pid}).

%------------------------------------------------------------------------------
% gen_server callbacks
%------------------------------------------------------------------------------

init([])->
    gen_server:cast(?MODULE, start),
    {ok, #?STATE{}}.

handle_call(nci_details, _From, State = #?STATE{
                                            communities = Communities,
                                            nci = NCI}) ->
    {reply, json_nci_details(NCI, Communities), State};
handle_call(Msg, From, State) ->
    error({no_handle_call, ?MODULE}, [Msg, From, State]).

handle_cast(start, State) ->
    StartTime = calendar:universal_time(),
    Time = list_to_binary(tap_time:rfc3339(StartTime)),
    LNCI = jiffy:encode({[{<<"Time">>, Time}, {<<"NCI">>, 1}]}),
    LNEP = jiffy:encode({[{<<"Time">>, Time}, {<<"NEP">>, 1}]}),
    LQPS = jiffy:encode({[{<<"Time">>, Time}, {<<"QPS">>, 1}]}),
    {noreply, State#?STATE{start_time = StartTime,
                      last_nci = LNCI,
                      nci_log = ets:new(nci_log, [ordered_set]),
                      last_nep = LNEP,
                      last_int_nep = 0,
                      last_qps = LQPS}};
handle_cast({num_endpoints, NEP, UT}, State = #?STATE{last_int_nep = LIntNEP,
                                                   clients = Clients}) ->
    NewState = case NEP =/= LIntNEP of
        true ->
            Time = list_to_binary(tap_time:rfc3339(UT)),
            JSON = jiffy:encode({[{<<"Time">>, Time}, {<<"NEP">>, NEP}]}),
            broadcast_msg(Clients, JSON),
            State#?STATE{last_nep = JSON, last_int_nep = NEP};
        false -> State
    end,
    {noreply, NewState};
handle_cast({nci, NCI, Communities, UT}, State = #?STATE{nci_log = NCILog,
                                                         clients = Clients}) ->
    true = ets:insert(NCILog, {UT, NCI}),
    Time = list_to_binary(tap_time:rfc3339(UT)),
    JSON = jiffy:encode({[{<<"Time">>, Time}, {<<"NCI">>, NCI}]}),
    broadcast_msg(Clients, JSON),
    {noreply, State#?STATE{last_nci = JSON,
                           nci = NCI,
                           communities = Communities}};
handle_cast({nci_details, Pid}, State = #?STATE{
                                            communities = Communities,
                                            nci = NCI}) ->
    clientsock:send(Pid, json_nci_details(NCI, Communities)),
    {noreply, State};
handle_cast({qps, QPS, UT}, State = #?STATE{clients = Clients}) ->
    Time = list_to_binary(tap_time:rfc3339(UT)),
    JSON = jiffy:encode({[{<<"Time">>, Time}, {<<"QPS">>, QPS}]}),
    broadcast_msg(Clients, JSON),
    {noreply, State#?STATE{last_qps = JSON}};
handle_cast({new_client, Pid}, State = #?STATE{clients = Clients,
                                               start_time = StartTime,
                                               last_nci = LNCI,
                                               last_nep = LNEP,
                                               last_qps = LQPS}) ->
    monitor(process, Pid),
    ?DEBUG("tap_client_data: new client ~p~n",[Pid]),
    HELLO = jiffy:encode({[{<<"start_time">>,
                                list_to_binary(tap_time:rfc3339(StartTime))},
                           {<<"current_time">>,
                                list_to_binary(tap_time:rfc3339(calendar:universal_time()))}]}),
    clientsock:send(Pid, HELLO),
    clientsock:send(Pid, LNCI),
    clientsock:send(Pid, LNEP),
    clientsock:send(Pid, LQPS),
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
                        [{<<"Time">>, list_to_binary(tap_time:rfc3339(Time))},
                         {<<"NCI">>,Value} | AccIn]
                    end, [], Data),
    JSON = jiffy:encode({JSONData}),
    clientsock:send(Pid, JSON).

broadcast_msg(Clients, Msg) ->
    [clientsock:send(C, Msg) || C <- Clients].

json_nci_details(NCI, Communities) ->
    jiffy:encode({[
        {<<"action">>,<<"NCIDetails">>},
        {<<"NCI">>,NCI},
        {<<"Time">>, list_to_binary(tap_time:rfc3339(calendar:universal_time()))},
        {<<"Communities">>, communities(Communities)}
    ]}).

communities({Endpoints, Interactions}) ->
    Communities = intersect_keys(Endpoints, Interactions),
    [
        {[
            {<<"Interactions">>, interactions(dict:fetch(C, Interactions))},
            {<<"Endpoints">>, endpoints(dict:fetch(C, Endpoints))}
        ]} || C <- Communities
    ].

intersect_keys(A, B) ->
    sets:to_list(sets:intersection(
        sets:from_list(dict:fetch_keys(A)),
        sets:from_list(dict:fetch_keys(B)))).

interactions(L) ->
    [[endpoint(A), endpoint(B)] || {A, B} <- L].

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
