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
%% @doc tap module

-module(tap_ds).

-compile([export_all]).

-record(state,{digraph,tap_client_data,nci_timestamp,nci_min_interval,cleaning_timestamp,data_max_age}).

start()->
    NCIMinInterval = get_config(nci_min_interval),
    DataMaxAge = days_to_seconds(get_config(data_max_age)),
    TapClientData = tap_client_data:start(),
    CurDateTime = calendar:universal_time(),
    {_CurDate,CurTime} = CurDateTime,
    CurSeconds = calendar:time_to_seconds(CurTime),
    Pid = spawn(?MODULE,listen,[#state{digraph = undefined,
				       tap_client_data=TapClientData,
				       nci_timestamp=CurSeconds,
				       nci_min_interval=NCIMinInterval,
				       cleaning_timestamp=CurDateTime,
				       data_max_age=DataMaxAge}]),
    Pid.


listen(State)->
    Digraph = State#state.digraph,
    case Digraph of
	undefined ->
	    register(tap_ds,self()),
	    listen(State#state{digraph = digraph:new()});
	_ -> ok
    end,
    receive
	{ordered_edge,OE}->
	    add_edges_and_clean(State,[OE]);
	{bulk_data,BulkData} when is_list(BulkData) ->
	    add_edges_and_clean(State,BulkData);
	Msg ->
	    io:format("Msg: ~p, ~p~n",[Msg,State]),
	    listen(State)
    end.


add_edges_and_clean(State,Edges)->
    Digraph = State#state.digraph,
    TapClientData = State#state.tap_client_data,
    NCITimeStamp = State#state.nci_timestamp,
    NCIMinInterval = State#state.nci_min_interval,
    CleaningTimeStamp = State#state.cleaning_timestamp,
    DataMaxAge = State#state.data_max_age,
    DateTime = calendar:universal_time(),
    {_Date,Time} = DateTime,
    lists:foreach(fun(X)->add_edge(Digraph,X,DateTime) end,Edges),
    TapClientData ! {num_endpoints,{digraph:no_vertices(Digraph),DateTime}},
    TimeInSeconds = calendar:time_to_seconds(Time),
    Elapsed = TimeInSeconds - NCITimeStamp,
%	    io:format("CleaningTimeStamp = ~p~nDateTime = ~p~nNCITimeStamp = ~p~nTimeInSeconds= ~p~nElapsed = ~p~nNCIMinInterval = ~p~n",[CleaningTimeStamp,
%																	  DateTime,
%																	  NCITimeStamp,
%																	  TimeInSeconds,
%																	  Elapsed,
%																	  NCIMinInterval]),
    case Elapsed > NCIMinInterval of
	true ->
	    spawn(?MODULE,get_nci,[TapClientData,Digraph]),
	    listen(State#state{nci_timestamp=TimeInSeconds});
	false ->
	    Age = calendar:time_difference(CleaningTimeStamp,DateTime),
	    AgeSeconds = days_to_seconds(Age),
%		    io:format("****~nAge = ~p~nAgeSeconds = ~p~nDataMaxAge = ~p~n",[Age,AgeSeconds,DataMaxAge]),
	    case AgeSeconds > DataMaxAge of
		true ->
		    clean(Digraph,DateTime,DataMaxAge),
		    listen(State#state{cleaning_timestamp=DateTime});
		false ->
		    listen(State)
	    end
    end.
    


clean(G,T,MaxAge)->
    Vertices = digraph:vertices(G),
    OldVertices = lists:filter(fun(X)->
				       {_,TS} = digraph:vertex(G,X),
				       Age = days_to_seconds(calendar:time_difference(TS,T)),
				       Age > MaxAge
			       end,
			       Vertices),
    error_logger:info_msg("~n**** Cleaning at Time ~p ****~nMaxAge = ~p~nStale Vertices = ~p~n****",[T,MaxAge,OldVertices]),
    digraph:del_vertices(G,OldVertices).


add_edge(G,E,Time)->
    {A,B} = E,
    case A =/= B of
	true ->
	    V1 = digraph:add_vertex(G,A,Time),
	    V2 = digraph:add_vertex(G,B,Time),
	    find_edge(G,V1,V2,Time),
	    find_edge(G,V2,V1,Time);
	false -> error
    end.

	  

get_nci(Pid,Digraph)->	  
    NCI = nci:compute_from_graph(Digraph),
    Pid ! {nci,{NCI,calendar:universal_time()}}.


find_edge(G,V1,V2,Time)->
    Edges = digraph:edges(G,V1),
    Found = lists:filter(fun(X)-> 
				 {_, FV1, FV2, _} = digraph:edge(G,X),
				 (V1 == FV1) and (V2 == FV2)
			 end,
			 Edges),
    case Found of
	[] ->
	    digraph:add_edge(G,V1,V2,Time);
	[E] -> digraph:add_edge(G,E,V1,V2,Time)
    end.
   

get_config(Value)-> 
    ConfigFile = file:consult("tapestry.config"),
    case ConfigFile of
	{ok,Config}->
	    process_config(Value,Config);
	_ -> {error,no_config}
    end.


process_config(_,[])->
    error;
process_config(Value,[Config|Rest]) ->
    case Config of
	{timers,Timers}->
	    process_timers(Value,Timers);
	_ -> process_config(Value,Rest)
    end.

process_timers(_,[])->
    error;
process_timers(nci_min_interval,[{nci_min_interval,{seconds,Time}}|_Rest]) ->
    Time;
process_timers(data_max_age,[{data_max_age,{{days,D},{hms,{H,M,S}}}}|_Rest]) ->
    {D,{H,M,S}};
process_timers(Value,[_H|Rest]) ->
    process_timers(Value,Rest).


days_to_seconds({D,{H,M,S}})->
   ( D * 24 * 60 * 60) + (H * 60 * 60) + (M * 60) + S.

