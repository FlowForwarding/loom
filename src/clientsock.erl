-module(clientsock).

-compile([export_all]).

-include("../include/tapestry.hrl").

handle_message({text, <<"PING">>}) ->
    Pid = self(),
    register(client,self()),
    error_logger:info_msg("Received PING:~n"),
    error_logger:info_msg("Client Registered as {client,~p}~n",[Pid]),
    {reply, {text, <<"PONG">>}};
handle_message(A)->
    error_logger:info_msg("Received:~n~p~n",[A]),
    error_logger:info_msg("My Pid = ~p~n",[self()]),
    noreply.

send(ID,Message) when is_atom(ID) ->
    Pid = whereis(ID),
    send(Pid,Message);
send(Pid,Message) when is_pid(Pid) ->
    Term = term_to_binary(Message),
    yaws_api:websocket_send(Pid, {binary, Term}).

