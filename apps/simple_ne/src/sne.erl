-module(sne).

-export([
    tr/0,
    grant_ipaddr/1,
    revoke_ipaddr/1,
    send/2,
    subscribe/2,
    ping/1,
    i/1
]).

tr() ->
    dbg:start(),
    dbg:tracer(),
    dbg:p(all, c),
    dbg:tpl(ofs_handler_logic, []),
    dbg:tpl(simple_ne_logic, []),
    dbg:tpl(simple_ne_ofsh, []).

grant_ipaddr(IpAddr) ->
    % use ofs_handler as callback module
    % ofs_handler calls us
    ok = of_driver:grant_ipaddr(IpAddr).

revoke_ipaddr(IpAddr) ->
    ok = of_driver:revoke_ipaddr(IpAddr).

send(IpAddr, Msg) ->
    simple_ne_logic:send(IpAddr, Msg).

subscribe(IpAddr, MsgType) ->
    simple_ne_logic:subscribe(IpAddr, MsgType).

ping(IpAddr) ->
    Echo = of_msg_lib:echo_request(4, <<>>),
    send(IpAddr, Echo).

i(switches) ->
    lists:foreach(
        fun({IpAddr, DatapathId, Version, _}) ->
            io:format("~p: v~p ~p~n", [IpAddr, Version, DatapathId])
        end, simple_ne_logic:switches()).
