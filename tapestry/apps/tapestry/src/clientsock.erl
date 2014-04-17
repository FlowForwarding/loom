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
%% @doc Client Socket module - callback from yaws

-module(clientsock).

-include("tap_logger.hrl").

-export([handle_message/1, send/2]).

%------------------------------------------------------------------------------
% API
%------------------------------------------------------------------------------

send(Pid,Message) when is_pid(Pid) ->
    ?DEBUG("Sending ~p to ~p~n",[Message,Pid]),
    yaws_api:websocket_send(Pid, {text, Message}).

%------------------------------------------------------------------------------
% yaws callback
%------------------------------------------------------------------------------

handle_message({text, <<"PING">>}) ->
    ?DEBUG("Received PING:~n"),
    {reply, {text, <<"PONG">>}};
handle_message({text, <<"{\"request\":\"start_data\"}">>})->
    handle_message({text, <<"START_DATA">>});
handle_message({text, <<"START_DATA">>}) ->
    handle_start_data(tap_config:getconfig(ui_test)),
    noreply;
handle_message({text, MessageBits}) when is_bitstring(MessageBits) ->
    ?DEBUG("Received:~p~n",[MessageBits]),
    decode(MessageBits),
    noreply;
handle_message({close, _, _})->
    % ignore close, not needed
    noreply;
handle_message(A)->
    ?INFO("UNKNOWN MESSAGE RECEIVED:~n~p~n",[A]),
    ?DEBUG("My Pid = ~p~n",[self()]),
    noreply.

%------------------------------------------------------------------------------
% local functions
%------------------------------------------------------------------------------

handle_start_data(enabled) ->
    tap_test_ui:new_client(self());
handle_start_data(disabled) ->
    tap_client_data:new_client(self()).

decode(MessageBits)->
    ?DEBUG("decoding..."),       
    try 
        % XXX might be in any order, sort first?
        Message = jiffy:decode(MessageBits),
        case Message of
            {[{<<"request">>, <<"more_data">>},
              {<<"start">>, Start},
              {<<"end">>, End},
              {<<"max_items">>, MaxData}]} ->
                % XXX log sending request
                tap_client_data:more_nci_data(self(), tap_time:rfc3339_to_epoch(binary_to_list(Start)), tap_time:rfc3339_to_epoch(binary_to_list(End)), list_to_integer(binary_to_list(MaxData)));
            _ -> 
               ?INFO("Unexpected Message:~p~n", [Message])
        end
    catch 
        Error ->
            ?WARNING("Parse Error: ~p  on~p~n", [Error, MessageBits])
    end.
