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
%% @doc Client Socket module

-module(clientsock).

-compile([export_all]).

-include("../include/tapestry.hrl").

handle_message({text, <<"PING">>}) ->
    Pid = self(),
    register(client,self()),
    error_logger:info_msg("Received PING:~n"),
    error_logger:info_msg("Client Registered as {client,~p}~n",[Pid]),
    {reply, {text, <<"PONG">>}};
handle_message({text, <<"START_DATA">>}) ->
    Pid = self(),
    random:seed(),
    error_logger:info_msg("Received START_DATA:~n"),
    DataPid = whereis(tap_client_data),
    case DataPid of
	undefined ->
	    spawn(tap_client_data,fake_nci_feed,[Pid]),
	    spawn(tap_client_data,fake_qps_feed,[Pid]),
	    spawn(tap_client_data,fake_nep_feed,[Pid]);
	_ when is_pid(DataPid) ->
	    DataPid ! {new_client,Pid}
    end,
    noreply;
handle_message({text, <<"{\"request\":\"start_data\"}">>})->
    handle_message({text, <<"START_DATA">>});
handle_message({text, MessageBits }) when is_bitstring(MessageBits ) ->
    error_logger:info_msg("Received:~p~n",[MessageBits]),
    decode(MessageBits),
    noreply;
handle_message({close,_,_})->
    DataPid = whereis(tap_client_data),
    Pid = self(),
    error_logger:info_msg("Closing client:~p~n",[Pid]),
    case DataPid of
	undefined ->
	    ok;
	_ when is_pid(DataPid) ->
	    DataPid ! {remove_client,Pid}
    end,
    noreply;
handle_message(A)->
    error_logger:info_msg("UNKNOWN MESSAGE RECEIVED:~n~p~n",[A]),
    error_logger:info_msg("My Pid = ~p~n",[self()]),
    noreply.

send(Pid,Message) when is_pid(Pid) ->
    error_logger:info_msg("Sending ~p to ~p~n",[Message,Pid]),
    yaws_api:websocket_send(Pid, {text, Message}).



decode(MessageBits)->
    error_logger:info_msg("decoding..."),	
    try 
	Message = jiffy:decode(MessageBits),
	case Message of
	    {[{<<"request">>,<<"more_data">>},
	      {<<"start">>,Start},
	      {<<"end">>,End},
	      {<<"max_items">>,MaxData}]} ->
		Request = {more_nci_data,self(),tap_utils:rfc3339_to_epoch(binary_to_list(Start)),tap_utils:rfc3339_to_epoch(binary_to_list(End)),list_to_integer(binary_to_list(MaxData))},
		error_logger:info_msg("Sending Request: ~p~n",[Request]),
		tap_client_data:send(Request);
	    _ ->  error_logger:info_msg("Unexpected Message:~p~n",[Message])
	end
    catch 
	Error ->
	    error_logger:info_msg("Parse Error: ~p  on~p~n",[Error,MessageBits])
    end.
