%%%-------------------------------------------------------------------
%%% @copyright (C) 1999-2013, Erlang Solutions Ltd
%%% @author Marc Sugiyama <marc.sugiyama@erlang-solutions.com>
%%% @doc
%%% Simple network executive command line utility functions.
%%% @end
%%%-------------------------------------------------------------------
-module(sne).
-copyright("2013, Erlang Solutions Ltd.").

-include_lib("ofs_handler/include/ofs_handler.hrl").
-include_lib("of_protocol/include/of_protocol.hrl").

-export([
    tr/0,
    send/2,
    sync_send/2,
    subscribe/2,
    ping/1,
    i/1
]).

%% @hidden
-spec tr() -> ok.
tr() ->
    dbg:start(),
    dbg:tracer(),
    dbg:p(all, c),
    dbg:tpl(ofs_handler_logic, []),
    dbg:tpl(simple_ne_logic, []),
    dbg:tpl(simple_ne_ofsh, []),
    ok.

%% @doc
%% Send a message to the switch at ``IpAddr''.  Responses are returned
%% via the ``handle_message'' callback.  To see the messages, you must
%% create a subscription to the expected replies.
%% @end
-spec send(ipaddress(), ofp_message()) -> not_found | ok | {error, error_reason()}.
send(IpAddr, Msg) ->
    simple_ne_logic:send(IpAddr, Msg).

%% @doc
%% Send a message to the switch at ``IpAddr'' and wait for the reply.
%% @end
-spec sync_send(ipaddress(), ofp_message()) -> not_found | {ok, ofp_message()} | {error, error_reason()}.
sync_send(IpAddr, Msg) ->
    simple_ne_logic:sync_send(IpAddr, Msg).

%% @doc
%% Add a subscription.
%% @end
-spec subscribe(ipaddress(), subscription_item()) -> ok.
subscribe(IpAddr, MsgType) ->
    simple_ne_logic:subscribe(IpAddr, MsgType).

%% @doc
%% send an echo request to the server.  To see the response, first create
%% a subscription to the echo response: ``sne:subscribe(IpAddr, echo_reply)''.
%% @end
-spec ping(ipaddress()) -> not_found | ok | {error, error_reason}.
ping(IpAddr) ->
    Echo = of_msg_lib:echo_request(4, <<>>),
    send(IpAddr, Echo).

%% @doc
%% Get information about simple_ne state.
%%
%% ``switches'' - list the ip address, protocol version, and datapath id
%% of the connected switches.
%% @end
-spec i(Item:: atom()) -> term().
i(switches) ->
    lists:foreach(
        fun({IpAddr, DatapathId, Version, _, _}) ->
            io:format("~p: v~p ~p~n", [IpAddr, Version, DatapathId])
        end, simple_ne_logic:switches()).
