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
    handle_start_data(tap_config:is_defined(test_ui, datasources)),
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

handle_start_data(true) ->
    tap_test_ui:new_client(self());
handle_start_data(_) ->
    tap_client_data:new_client(self()).

decode(MessageBits)->
    try 
        Message = jiffy:decode(MessageBits),
        case Message of
            {Request} ->
                process_request(Request);
            _ -> 
               ?INFO("Unexpected Message:~p~n", [Message])
        end
    catch 
        Error ->
            ?WARNING("Parse Error: ~p  on~p~n", [Error, MessageBits])
    end.

process_request(Message) ->
    Msg = fun(Key) -> proplists:get_value(Key, Message) end,

    % pick out command (either request or action)
    Request = Msg(<<"request">>),
    Action = Msg(<<"action">>),
    case {Request, Action} of
        {<<"more_data">>, _} ->
            End = Msg(<<"end">>),
            Start = Msg(<<"start">>),
            MaxItems = Msg(<<"max_items">>),
            ?DEBUG("action more_data: start ~p end ~p max_items ~p",
                                        [Start, End, MaxItems]),
            tap_client_data:more_nci_data(self(),
                                          rfc3339_to_epoch(Start),
                                          rfc3339_to_epoch(End),
                                          binary_to_integer(MaxItems));
        {_, <<"NCIDetails">>} ->
            ?DEBUG("action NCIDetails"),
            tap_client_data:nci_details(self());
        {_, <<"collectors">>} ->
            ?DEBUG("action collectors"),
            tap_client_data:collectors(self());
        {_, <<"getconfig">>} ->
            ?DEBUG("getconfig"),
            tap_client_data:getconfig(self());
        {_, <<"getlimits">>} ->
            ?DEBUG("action getlimits"),
            tap_client_data:limits(self());
        {_, <<"setlimits">>} ->
            {LimitElement} = Msg(<<"limits">>),
            MsgLimit = fun(Limit) ->
                        proplists:get_value(Limit, LimitElement) end,
            Limits = [{max_vertices, MsgLimit(<<"max_vertices">>)},
                      {max_edges, MsgLimit(<<"max_edges">>)},
                      {comm_size_limit, MsgLimit(<<"comm_size_limit">>)},
                      {max_communities, MsgLimit(<<"max_communities">>)}],
            ?DEBUG("action setlimits ~p", [Limits]),
            tap_client_data:setlimits(self(), Limits);
        {_, _} -> 
           ?INFO("Unexpected Message:~p~n", [Message])
    end.

rfc3339_to_epoch(B) when is_binary(B) ->
    tap_time:rfc3339_to_epoch(binary_to_list(B)).
