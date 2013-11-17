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
%% @doc Tap Aggregator module

-module(tap_aggr).

-compile([export_all]).

-record(state,{raw_edge_list,tap_ds,tap_client_data,query_count,time_stamp}).
    

start(Time,Interval,MsgTime)->
    Receivers = tap_loom:get_ofdp_recv_list(),
    case Receivers of
	[] -> timer:sleep(Interval * 1000),
	      case Rem = Time rem MsgTime of
		  Rem when Rem == 0 ->
		      error_logger:info_msg("tap_aggr: waiting for data recievers for ~p seconds...~n",[Time]),
		      start(Time+Interval,Interval,MsgTime);
		  _ -> start(Time+Interval,Interval,MsgTime)
		  end;
	_ -> error_logger:info_msg("tap_aggr: starting...~n"),
	     TapDS = tap_ds:start(),
	     TCD = whereis(tap_client_data),
	     {_Date,CurTime} = calendar:universal_time(),
	     Pid = spawn(?MODULE,listen,[#state{raw_edge_list=[],tap_ds = TapDS,tap_client_data=TCD,query_count=0,time_stamp=calendar:time_to_seconds(CurTime)}]),
	     register(tap_aggr,Pid),
	     [ Recv ! {subscribe, {Pid, packet_in_dns_reply}} || Recv <- Receivers ],
	     Pid
    end.




listen(State)->
    receive
	{dns_reply,Reply}->
	    RawEdgeList = State#state.raw_edge_list,
	    TapDS = State#state.tap_ds,
	    LastTimeStamp = State#state.time_stamp,
	    QueryCount = State#state.query_count,
	    {CurDate,CurTime} = calendar:universal_time(),
	    CurTimeStamp = calendar:time_to_seconds(CurTime),
	    NewRawEdgeList= [Reply|RawEdgeList],
	    OR = dns_reply_order(Reply),
	    TapDS ! {ordered_edge,OR},
	    Interval = CurTimeStamp - LastTimeStamp,
	    NewState = case (Interval > 30) or (QueryCount > 9999) of
			   true ->
			       TCD = State#state.tap_client_data,
			       QPS = QueryCount / Interval,
			       TCD ! {qps,{QPS,{CurDate,CurTime}}},
			       State#state{raw_edge_list = NewRawEdgeList,query_count=0,time_stamp=CurTimeStamp};
			   false -> 
			       State#state{raw_edge_list = NewRawEdgeList,query_count=QueryCount+1}
		       end,
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
