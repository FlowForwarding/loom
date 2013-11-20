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
%% @doc Tap client data

-module(tap_client_data).

-compile([export_all]).

-include("../include/tapestry.hrl").

-record(state,{start_time,clients,last_nci,nci_log,last_nep,last_int_nep,last_qps}).

start()->
    StartTime = calendar:universal_time(),
    Time = list_to_binary(tap_utils:rfc3339(StartTime)),
    LNCI = jiffy:encode({[{<<"Time">>,Time},{<<"NCI">>,1}]}),
    LNEP = jiffy:encode({[{<<"Time">>,Time},{<<"NEP">>,1}]}),
    LQPS = jiffy:encode({[{<<"Time">>,Time},{<<"QPS">>,1}]}),
    Pid = spawn(?MODULE,listen,[#state{start_time=StartTime,clients=[],last_nci=LNCI,nci_log=undefined,last_nep=LNEP,last_int_nep=0,last_qps=LQPS}]),
    Pid.

send(Msg) when is_atom(Msg); is_tuple(Msg) ->
    DataPid = whereis(tap_client_data),
    case DataPid of
	unknown ->
	    error_logger:info_msg("no tap_client_data process");
	_ when is_pid(DataPid) -> 
	    DataPid ! Msg
    end.

listen(State)->
    NCILog = State#state.nci_log,
    case NCILog of
	undefined ->
	    register(tap_client_data,self()),
	    listen(State#state{nci_log = ets:new(nci_log,[])});
	_ -> ok
    end,
    Clients = State#state.clients,
%    NCILog = State#state.nci_log,
    StartTime = State#state.start_time,
    LNCI = State#state.last_nci,
    LNEP = State#state.last_nep,
    LQPS = State#state.last_qps,
    LIntNEP = State#state.last_int_nep,
    receive
	{num_endpoints,Data}->
	    {NEP,UT} = Data,
	    case NEP =/= LIntNEP of
		true ->
		    Time = list_to_binary(tap_utils:rfc3339(UT)),
		    JSON = jiffy:encode({[{<<"Time">>,Time},{<<"NEP">>,NEP}]}),
		    NewClients = broadcast_msg(Clients,JSON),
		    NewState = State#state{clients=NewClients,last_nep=JSON,last_int_nep=NEP},
		    listen(NewState);
		false -> listen(State)
	    end;
	{nci,Data}->
	    {NCI,UT} = Data,
	    ets:insert(NCILog,{UT,NCI}),
%	    NewNCILog = [{UT,NCI}|NCILog],
	    Time = list_to_binary(tap_utils:rfc3339(UT)),
	    JSON = jiffy:encode({[{<<"Time">>,Time},{<<"NCI">>,NCI}]}),
	    NewClients = broadcast_msg(Clients,JSON),
	    NewState = State#state{clients=NewClients,last_nci=JSON,nci_log=NCILog},
	    listen(NewState);
	{qps,Data}->
	    {QPS,UT} = Data,
	    Time = list_to_binary(tap_utils:rfc3339(UT)),
	    JSON = jiffy:encode({[{<<"Time">>,Time},{<<"QPS">>,QPS}]}),
	    NewClients = broadcast_msg(Clients,JSON),
	    NewState = State#state{clients=NewClients,last_qps=JSON},
	    listen(NewState);
	{new_client,Pid} ->
	    NewClients = [Pid|Clients],
	    io:format("tap_client_data: added client Pid =  ~p~n",[Pid]),
	    HELLO = jiffy:encode({[{<<"start_time">>,list_to_binary(tap_utils:rfc3339(StartTime))},{<<"current_time">>,list_to_binary(tap_utils:rfc3339(calendar:universal_time()))}]}),
	    clientsock:send(Pid,HELLO),
	    clientsock:send(Pid,LNCI),
	    clientsock:send(Pid,LNEP),
	    clientsock:send(Pid,LQPS),
	    NewState = State#state{clients=NewClients},
	    listen(NewState);
	{remove_client,Pid} ->
	    NewClients = lists:delete(Pid,Clients),
	    NewState = State#state{clients=NewClients},
	    listen(NewState);
	{more_nci_data,Pid,Start,End,MaxData} ->
	    error_logger:info_msg("tap_client_data: {more_nci_data,~p,~p,~p,~p}~n",[Pid,Start,End,MaxData]),
	    spawn(fun()->
%			  Target = lists:filter(fun({Time,_Value})->(Time >= Start) and (Time =< End) end,NCILog),
			  Target = ets:foldl(fun(X,AccIn)->{Time,_Value} = X,
							   case (Time >= Start) and (Time =< End) of
							       true -> [X|AccIn];
							       false -> AccIn
							   end
					     end, [], NCILog),
			  Length = length(Target),
			  case Length > 0 of
			      true ->
				  case (Length div MaxData) of
				      Step when Step =< 1 ->
					  send_more_data(Pid,Target);
				      Step when Step > 1 ->
					  {Subset,_} = lists:mapfoldl(fun(X,AccIn)->
									      case (AccIn rem Step) of
										  0 -> {X,AccIn+1};
										  _ -> {false,AccIn+1}
									      end 
								      end,0,Target),
					  NewTarget = lists:filter(fun(X)->X /= false end,Subset),
					  send_more_data(Pid,NewTarget)
				  end;
			      false -> error_logger:info_msg("tap_client_data: NO RESULTS (i.e. empty set) for {more_nci_data,...} request from ~p~n",[Pid])
			  end
		  end),
	    listen(State);
	Msg ->
	    io:format("Msg: ~p~n",[Msg]),
	    listen(State)
    end.

broadcast_msg(Clients,Msg)->
    lists:foldl(
      fun(Pid,AccIn)->
	      case is_pid(Pid) of 
		  true ->
		      clientsock:send(Pid,Msg),
		      [Pid|AccIn];
		  false ->
		      AccIn
	      end
      end,[],Clients).


send_more_data(Pid,Data) when is_pid(Pid), is_list(Data)->
    error_logger:info_msg("tap_client_data: sending more data ~p to ~p~n",[Data,Pid]),
    JSONData = lists:foldl(fun({Time,Value},AccIn)->[{<<"Time">>,list_to_binary(tap_utils:rfc3339(Time))},{<<"NCI">>,Value}|AccIn] end,[],Data),
    JSON = jiffy:encode({JSONData}),
    clientsock:send(Pid,JSON).
    

fake_nci_feed(Pid)->
    Wait = random:uniform(4) * 500,
    NCI = random:uniform(100),
    Time = list_to_binary(tap_utils:rfc3339(erlang:universaltime())),
    JSON = jiffy:encode({[{<<"Time">>,Time},{<<"NCI">>,NCI}]}),
    clientsock:send(Pid,JSON),
    timer:sleep(Wait),
    fake_nci_feed(Pid).

fake_nci_feed2(Pid)->
    Wait = random:uniform(4) * 500,
    NCI = random:uniform(100),
    Time = calendar:universal_time(),
    Pid ! {nci,{NCI,Time}},
    timer:sleep(Wait),
    fake_nci_feed2(Pid).

fake_qps_feed(Pid)->
    Wait = random:uniform(2) * 500,
    QPS = random:uniform(2000000) + 1000000,
    Time = list_to_binary(tap_utils:rfc3339(erlang:universaltime())),
    JSON = jiffy:encode({[{<<"Time">>,Time},{<<"QPS">>,QPS}]}),
    clientsock:send(Pid,JSON),
    timer:sleep(Wait),
    fake_qps_feed(Pid).

fake_nep_feed(Pid)->
    Wait = random:uniform(10) * 500,
    NEP = random:uniform(50000) + 200000,
    Time = list_to_binary(tap_utils:rfc3339(erlang:universaltime())),
    JSON = jiffy:encode({[{<<"Time">>,Time},{<<"NEP">>,NEP}]}),
    clientsock:send(Pid,JSON),
    timer:sleep(Wait),
    fake_nep_feed(Pid).
    

