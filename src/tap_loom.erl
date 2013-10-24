-module(tap_loom).

-compile([export_all]).



get_ofdp_recv_list()->
    LoomSupTree = loom:get_sup_tree(),
    TapLoom = lists:keyfind(dns_tap,1,LoomSupTree),
    TapChildren = case TapLoom of
		      false ->
			  tapestry_not_running;
		      {dns_tap,_,Children} -> Children
		  end,
    OFDPL = case TapChildren of
		tapestry_not_running ->
		    false;
		[] -> false;
		_ -> {OFDP,Rest} = lists:partition(fun(X)->
							   [Name | Rest] = tuple_to_list(X),
							   Name == loom_odfp_recv end,TapChildren),
		     OFDP
	    end,
    case OFDPL of
	[] -> false;
	_ -> OFDPL
    end.
	     
				     
