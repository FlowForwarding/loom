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
