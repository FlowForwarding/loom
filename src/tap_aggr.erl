-module(tap_aggr).

-compile([export_all]).


start()->
    Pid = spawn(?MODULE,listen,[]),
    Pid.


listen(State)->
    receive
	Msg ->
	    io:format("Msg: ~p~n",[Msg]),
	    listen(State)
    end.
	  
