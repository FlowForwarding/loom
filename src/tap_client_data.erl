-module(tap_client_data).

-compile([export_all]).

-include("../include/tapestry.hrl").


fake_nci_feed(Pid)->
    random:seed(),
    Wait = random:uniform(4) * 500,
    NCI = random:uniform(100),
    Time = list_to_binary(tap_utils:rfc3339(erlang:universaltime())),
    JSON = jiffy:encode({[{<<"Time">>,Time},{<<"NCI">>,NCI}]}),
    clientsock:send(Pid,JSON),
    timer:sleep(Wait),
    fake_nci_feed(Pid).

fake_qps_feed(Pid)->
    random:seed(),
    Wait = random:uniform(2) * 500,
    QPS = random:uniform(2000000) + 1000000,
    Time = list_to_binary(tap_utils:rfc3339(erlang:universaltime())),
    JSON = jiffy:encode({[{<<"Time">>,Time},{<<"QPS">>,QPS}]}),
    clientsock:send(Pid,JSON),
    timer:sleep(Wait),
    fake_qps_feed(Pid).

fake_nep_feed(Pid)->
    random:seed(),
    Wait = random:uniform(10) * 500,
    NEP = random:uniform(50000) + 200000,
    Time = list_to_binary(tap_utils:rfc3339(erlang:universaltime())),
    JSON = jiffy:encode({[{<<"Time">>,Time},{<<"NEP">>,NEP}]}),
    clientsock:send(Pid,JSON),
    timer:sleep(Wait),
    fake_nep_feed(Pid).
    

