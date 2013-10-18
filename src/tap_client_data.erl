-module(tap_client_data).

-compile([export_all]).

-include("../include/tapestry.hrl").


fake_feed(Pid)->
    Wait = random:uniform(4) * 500,
    NCI = random:uniform(100),
    Time = list_to_binary(tap_utils:rfc3339(erlang:universaltime())),
    JSON = jiffy:encode({[{<<"Time">>,Time},{<<"NCI">>,NCI}]}),
    clientsock:send(Pid,JSON),
    timer:sleep(Wait),
    fake_feed(Pid).
    

