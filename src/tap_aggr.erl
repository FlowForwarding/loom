-module(tap_aggr).

-compile([export_all]).


start()->
    Receivers = tap_loom:get_ofdp_recv_list(),
    case Receivers of
	[] -> timer:sleep(2000),
	      io:format("tap_aggr: waiting for data recievers...~n"),
	      start();
	_ -> io:format("tap_aggr: starting...~n"),
	     Pid = spawn(?MODULE,listen,[[]]),
	     [ Recv ! {subscribe, {Pid, packet_in_dns_reply}} || Recv <- Receivers ],
	     Pid
    end.


listen(State)->
    receive
	Msg ->
	    io:format("Msg: ~p~n",[Msg]),
	    listen(State)
    end.
	  
