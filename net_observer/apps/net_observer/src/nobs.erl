-module(nobs).
-export([graphs/0,
         switches/0,
         switch/1
]).

graphs() ->
    net_observer_logic:graphs().

switches() ->
    net_observer_logic:switches().

switch(DPID) ->
    net_observer_logic:switch(DPID).