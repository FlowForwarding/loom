-module(simple_ne).

-export([
    tr/0,
    grant_ipaddr/1,
    revoke_ipaddr/1
]).

tr() ->
    dbg:start(),
    dbg:tracer(),
    dbg:p(all, c),
    dbg:tpl(ofs_handler_driver, []),
    dbg:tpl(ofs_handler_logic, []),
    dbg:tpl(simple_ne_ofsh, []).

grant_ipaddr(IpAddr) ->
    % use ofs_handler as callback module
    % ofs_handler calls us
    ok = of_driver:grant_ipaddr(IpAddr).

revoke_ipaddr(IpAddr) ->
    ok = of_driver:revoke_ipaddr(IpAddr).
