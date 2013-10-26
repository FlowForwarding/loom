-module(tap_utils).

-compile([export_all]).

rfc3339({{Year, Month, Day}, {Hour, Minute, Second}})->
    SY = integer_to_list(Year),
    SM = integer_to_list(Month),
    SD = integer_to_list(Day),
    Sh = integer_to_list(Hour),
    Sm = integer_to_list(Minute),
    Ss = integer_to_list(Second),
    %Stamp = SY ++ "-" ++ SM ++ "-" ++ SD ++ "T" ++ Sh ++ ":" ++ Sm ++ ":" ++ Ss ++ "Z".
    Stamp =  io_lib:format("~4.4.0w-~2.2.0w-~2.2.0wT~2.2.0w:~2.2.0w:~2.2.0wZ", [ Year, Month, Day, Hour, Minute, Second]),
    lists:flatten(Stamp).

rfc3339_to_epoch(Timestamp)->
    {ok,[Year,Month,Day,Hour,Minute,Second],[]} = io_lib:fread("~4d-~2d-~2dT~2d:~2d:~2dZ",Timestamp),
    {{Year,Month,Day},{Hour, Minute, Second}}.
    
    
