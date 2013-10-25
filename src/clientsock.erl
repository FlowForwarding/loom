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
    error_logger:info_msg("Received:~n~p~n",[A]),
    error_logger:info_msg("My Pid = ~p~n",[self()]),
    noreply.

send(Pid,Message) when is_pid(Pid) ->
    error_logger:info_msg("Sending ~p to ~p~n",[Message,Pid]),
    yaws_api:websocket_send(Pid, {text, Message}).

