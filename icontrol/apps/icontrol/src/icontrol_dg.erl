-module(icontrol_dg).

-include("icontrol_logger.hrl").

-define(STATE,icontrol_dg_state).
-record(?STATE,{record = false,
                graph_name }).

-export([init/0,
         add_switch/4
        ]).

% Q: should i control have the remote node in a sys.config entry ?
-define(N,'ofs_store@127.0.0.1').

init() ->
    % ?INFO("(~p) Trying to ping ~p...\n",[?MODULE,?N]),
    % try 
    %     case net_adm:ping(?N) of
    %         pong -> 
    %             ?INFO("(~p) ping successfull...\n",[?MODULE]),
    %             % Q: Would icontrol need a new graph PER session, meaning if it goes down or was closed ?
    %             rpc_call(#?STATE{},new,[]);
    %         pang -> 
    %             #?STATE{}
    %     end
    % catch
    %     C:E ->
    %         ?ERROR("(~p) icontrol digraph node (~p) not present\n~p:~p\n~p\n",
    %             [?MODULE,?N,C,E,erlang:get_stacktrace()])
    % end.
    ok.

add_switch(_,_,_,_) ->
    ok.

% add_switch(#?STATE{ record = false } = State,Name,Labels,Alias) ->
%     % Q: should i check if the node has become available , and re-init ?
%     State;
% add_switch(#?STATE{ record = true, graph_name = GN } = State,Name,Labels,Alias) ->
%     rpc_call(State,add_vertex,[GN,{Name,Labels,Alias}]).
%%--------------------------------------------------------------------------------
rpc_call(State,F,A) ->
    case rpc:call(?N, dg, F, A) of
        {badrpc, Reason} ->
            % If the ofs_store node drops, should i still hold onto the graphname
            ?WARNING("(~p) badrpc ~p \n",[?MODULE,Reason]),
            State#?STATE{record=false};
        Result when F =:= new ->
            {ok,GN} = Result,
            State#?STATE{record = true, graph_name = GN};
        true ->
            State;
        false ->
            State;
        _ ->
            % Q: Should i log errors, unknown responses ?
            State
    end.
