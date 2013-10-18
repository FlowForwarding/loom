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
    error_logger:info_msg("Received START_DATA:~n"),
    spawn(tap_client_data,fake_feed,[Pid]),
    noreply;
handle_message(A)->
    error_logger:info_msg("Received:~n~p~n",[A]),
    error_logger:info_msg("My Pid = ~p~n",[self()]),
    noreply.

send(Pid,Message) when is_pid(Pid) ->
    yaws_api:websocket_send(Pid, {text, Message}).

