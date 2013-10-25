-module(tap_loom).

-compile([export_all]).

config()->
    ConfigFile = file:consult("tapestry.config"),
    case ConfigFile of
	{ok,Config}->
	    process_config(Config);
	_ -> {error,no_config}
    end.

process_config([])->
    ok;
process_config([Config|Rest]) ->
    case Config of
	{ofdps,_}->
	    process_ofdps(Config);
	_ -> process_config(Rest)
    end.

process_ofdps([])->
    ok;
process_ofdps([OFDP|Rest]) ->
    case OFDP of
	{ofdp,IP,DNSPort,ClientPort,DNSIps}->
	    {ip_addr,IPAddr} = IP,
	    {dns_port,Port1} = DNSPort,
	    {client_port,Port2} = ClientPort,
	    {dns_ips,IPs} = DNSIps,
	    OFDPList = loom:get_all(default),
	    lists:foreach(fun(X)->
				  {OFDPIP,_} = loom_ofdp:get_address(X),
				  case OFDPIP == IPAddr of
				      true ->
					  dns_tap([X],Port1,Port2,IPs);
				      false -> ok
				  end
			  end, OFDPList),
	    process_ofdps(Rest);
	_ -> process_ofdps(Rest)
    end.

    


dns_tap([],_Port1,_Port2,_IPTupleList)->
    ok;
dns_tap(OFDPL,Port1,Port2,IPTupleList)->
    [OFDP|Rest] = OFDPL,
    IPList = [ list_to_binary(tuple_to_list(IPTuple)) || IPTuple <- IPTupleList ],
    loom_ofdp_lib:clear(OFDP),
    [ send_dns_tap_msg(Port1,Port2,controller, IPv4Src, OFDP) || IPv4Src <- IPList ],
    loom_ofdp_lib:forward(OFDP,Port2,[Port1]), 
    loom_ofdp_lib:forward(OFDP, Port1,[Port2]),
    dns_tap(Rest,Port1,Port2,IPTupleList).

send_dns_tap_msg(Port1, Port2, Port3, IPv4Src, OFDP) ->    
    M1 = loom_flow_lib:tap_dns_response(Port1, Port2, Port3, IPv4Src),
    loom_ofdp:send_ofp_msg(OFDP, M1).



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
		_ -> {OFDP,_Rest} = lists:partition(fun(X)->
							   [Name | _Rest] = tuple_to_list(X),
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
