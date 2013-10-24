-module(tap_aggr).

-compile([export_all]).

-record(state,{raw_edge_list,tap_ds}).

start()->
    Receivers = tap_loom:get_ofdp_recv_list(),
    case Receivers of
	[] -> timer:sleep(2000),
	      io:format("tap_aggr: waiting for data recievers...~n"),
	      start();
	_ -> io:format("tap_aggr: starting...~n"),
	     TapDS = tap_ds:start(),
	     Pid = spawn(?MODULE,listen,[#state{tap_ds = TapDS}]),
	     register(tap_aggr,Pid),
	     [ Recv ! {subscribe, {Pid, packet_in_dns_reply}} || Recv <- Receivers ],
	     Pid
    end.


listen(State)->
    RawEdgeList = State#state.raw_edge_list,
    TapDS = State#state.tap_ds,
    receive
	{dns_reply,Reply}->
	    NewRawEdgeList= [Reply|RawEdgeList],
	    OR = dns_reply_order(Reply),
	    TapDS ! {ordered_edge,OR},
	    NewState = State#state{raw_edge_list = NewRawEdgeList},
	    listen(NewState);
	Msg ->
	    io:format("Msg: ~p~n",[Msg]),
	    listen(State)
    end.


test_raw_edge_list()->
    [{{10,48,33,190},{74,125,239,40}},
     {{10,48,33,190},{17,172,232,77}},
     {{10,48,33,190},{23,56,125,15}},
     {{10,48,33,190},{23,45,87,120}},
     {{10,48,33,190},{23,56,125,15}},
     {{10,48,33,190},{74,125,20,99}},
     {{10,48,33,190},{17,172,232,79}},
     {{10,48,33,190},{205,234,19,68}},
     {{10,48,33,190},{205,234,19,68}},
     {{10,48,33,190},{17,149,34,63}},
     {{10,48,33,190},{205,234,19,68}},
     {{10,48,33,190},{17,172,232,63}},
     {{10,48,33,190},{17,172,232,97}},
     {{10,48,33,190},{17,172,232,221}},
     {{10,48,33,190},{74,125,239,47}},
     {{10,48,33,190},{74,125,20,103}},
     {{10,48,33,190},{74,125,239,47}},
     {{10,48,33,190},{157,56,108,82}},
     {{10,48,33,190},{91,190,218,68}},
     {{10,48,33,190},{91,190,218,68}},
     {{10,48,33,190},{205,234,19,68}},
     {{10,48,33,190},{205,234,19,68}},
     {{10,48,33,190},{205,234,19,68}},
     {{10,48,33,190},{74,125,20,189}},
     {{10,48,33,190},{66,196,120,100}},
     {{10,48,33,190},{74,125,28,189}},
     {{10,48,33,190},{67,195,141,201}},
     {{10,48,33,190},{74,125,239,34}},
     {{10,48,33,190},{134,170,18,79}}].


dns_reply_order({A,B}=R)->
    case A < B of
	true ->
	    R;
	false -> {B,A}
    end.
