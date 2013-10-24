-module(tap_client_data).

-compile([export_all]).

-include("../include/tapestry.hrl").

-record(state,{clients}).

start()->
    Pid = spawn(?MODULE,listen,[#state{clients=[]}]),
    register(tap_client_data,Pid),
    Pid.

listen(State)->
    Clients = State#state.clients,
    receive
	{num_endpoints,Data}->
	    {NEP,UT} = Data,
	    Time = list_to_binary(tap_utils:rfc3339(UT)),
	    JSON = jiffy:encode({[{<<"Time">>,Time},{<<"NEP">>,NEP}]}),
	    NewClients = lists:foldl(
			   fun(Pid,AccIn)->
				   case is_pid(Pid) of 
				       true ->
					   clientsock:send(Pid,JSON),
					   [Pid|AccIn];
				       false ->
					   AccIn
				   end
			   end,[],Clients),
	    NewState = State#state{clients=NewClients},
	    listen(NewState);
	{new_client,Pid} ->
	    NewClients = [Pid|Clients],
	    io:format("tap_client_data: added client Pid =  ~p~n",[Pid]),
	    NewState = State#state{clients=NewClients},
	    listen(NewState);
	Msg ->
	    io:format("Msg: ~p~n",[Msg]),
	    listen(State)
    end.
					  
				 



fake_nci_feed(Pid)->
    Wait = random:uniform(4) * 500,
    NCI = random:uniform(100),
    Time = list_to_binary(tap_utils:rfc3339(erlang:universaltime())),
    JSON = jiffy:encode({[{<<"Time">>,Time},{<<"NCI">>,NCI}]}),
    clientsock:send(Pid,JSON),
    timer:sleep(Wait),
    fake_nci_feed(Pid).

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
    

