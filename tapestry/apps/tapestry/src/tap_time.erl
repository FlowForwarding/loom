-module(tap_time).

-export([now/0,
         diff/2,
         universal/1]).

now() ->
    os:timestamp().

diff(A, B) ->
    timer:now_diff(A, B) div 1000000.

universal(T) ->
    calendar:now_to_universal_time(T).
