-module(tap_loom).

-compile([export_all]).



get_ofdp_recv_list()->
    LoomSupTree = loom:get_sup_tree(),
    get_ofdp_recv_list(LoomSupTree).

get_ofdp_recv_list(LoomSupTree)->
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
							   Name == loom_ofdp_recv_sup end,TapChildren),
		     OFDP
	    end,
    Workers = case OFDPL of
		  [] -> false;
		  [{_,_,W}] -> W;
		  _ -> false
	      end,
    lists:foldl(fun(X,AccIn)->case X of
				  {_,Pid,worker,[loom_ofdp_recv]} ->
				      [Pid|AccIn];
				  _ -> AccIn
			      end
		end,[],Workers).
	
	     
				     
get_ofdp_recv_list_test()->
    LoomSupTree = [{dns_tap,{dns_tap,"<0.111.0>",supervisor,
                   [loom_controller_sup]},
          [{loom_ofdp_recv_sup,{loom_ofdp_recv_sup,"<0.115.0>",
                                                   supervisor,
                                                   [loom_ofdp_recv_sup]},
                               [{undefined,"<0.121.0>",worker,[loom_ofdp_recv]}]},
           {loom_ofdp_sup,{loom_ofdp_sup,"<0.114.0>",supervisor,
                                         [loom_ofdp_sup]},
                          [{undefined,"<0.119.0>",worker,[loom_ofdp]}]},
           {loom_c_listen_sup,{loom_c_listen_sup,"<0.113.0>",supervisor,
                                                 [loom_c_listen_sup]},
                              [{undefined,"<0.116.0>",worker,[loom_c_listen]}]},
           {loom_controller,"<0.112.0>",worker,[loom_controller]}]},
 {default,{default,"<0.105.0>",supervisor,
                   [loom_controller_sup]},
          [{loom_ofdp_recv_sup,{loom_ofdp_recv_sup,"<0.109.0>",
                                                   supervisor,
                                                   [loom_ofdp_recv_sup]},
                               [{undefined,"<0.120.0>",worker,[loom_ofdp_recv]}]},
           {loom_ofdp_sup,{loom_ofdp_sup,"<0.108.0>",supervisor,
                                         [loom_ofdp_sup]},
                          [{undefined,"<0.118.0>",worker,[loom_ofdp]}]},
           {loom_c_listen_sup,{loom_c_listen_sup,"<0.107.0>",supervisor,
                                                 [loom_c_listen_sup]},
                              [{undefined,"<0.110.0>",worker,[loom_c_listen]}]},
           {loom_controller,"<0.106.0>",worker,[loom_controller]}]}],
   get_ofdp_recv_list(LoomSupTree).
