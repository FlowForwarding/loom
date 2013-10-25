-module(tap_ds).

-compile([export_all]).

-record(state,{tap_client_data,edge_dict,endpoint_dict}).

start()->
    TapClientData = tap_client_data:start(),
    Pid = spawn(?MODULE,listen,[#state{edge_dict=dict:new(),endpoint_dict=dict:new(),tap_client_data=TapClientData}]),
    Pid.


listen(State)->
    TapClientData = State#state.tap_client_data,
    Edges = State#state.edge_dict,
    Endpoints = State#state.endpoint_dict,
    receive
	{ordered_edge,OE}->
	    {A,B} = OE,
	    Time = calendar:universal_time(),
	    NewEdges = dict:store(OE,Time,Edges),
	    NewEndpoints1 = dict:store(A,Time,Endpoints),
	    NewEndpoints2 = dict:store(B,Time,NewEndpoints1),
	    TapClientData ! {num_endpoints,{dict:size(NewEndpoints2),calendar:universal_time()}},
	    EdgeList = dict:to_list(NewEdges),
	    NCIinput = [ Edge || {Edge,_Time} <- EdgeList ],
	    spawn(?MODULE,get_nci,[TapClientData,NCIinput]),
	    NewState = State#state{edge_dict=NewEdges,endpoint_dict=NewEndpoints2},
	    listen(NewState);
	Msg ->
	    io:format("Msg: ~p, ~p~n",[Msg,State]),
	    listen(State)
    end.
	  

get_nci(Pid,EdgeList)->	  
    NCI = nci:compute(EdgeList),
    Pid ! {nci,{NCI,calendar:universal_time()}}.
