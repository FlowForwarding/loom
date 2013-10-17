%%------------------------------------------------------------------------------
%% Copyright 2013 Infoblox Inc.
%%
%%-----------------------------------------------------------------------------

%% @author Infoblox Inc <info@infoblox.com>
%% @copyright 2013 Infoblox.com
%% @doc Tapestry.  A Network Complexity Analyzer

-module(tapestry).


-compile([all]).

-export([nci_from_log_lines/1, nci_from_benchmark_data/1]).
%%%
% Reads DNS response log lines and processes them
%%%
nci_from_log_lines(FileName) ->
    {ok, File} = file:open(FileName, [read]),
    {NumLines, NumProcessed, Links} = links_from_log(File, 0, 0, []),
    file:close(File),
    NCI = nci:compute(Links),
    io:format("NumLines = ~p, NumProcessed = ~p NCI = ~p~n", [NumLines, NumProcessed, NCI]).

links_from_log(File, NumLines, NumProcessed, Links) ->
    case io:get_line(File, "") of
        eof  -> {NumLines, NumProcessed, Links};
        Line ->            
            case re:run(Line, "IN TXT", []) of
                nomatch ->
                    Link = process_line(Line),
                    links_from_log(File, NumLines+1, NumProcessed+1, [Link|Links]);
                _ ->
                    links_from_log(File, NumLines+1, NumProcessed, Links)                
            end        
    end.
    
process_line(Line) ->
%    io:format("In processLine~n"),
    [L2 |_] = re:split(Line, " IN "),
%%%  [L4| L3] = re:split(L2, ": UDP:"),         with the #value included
    [L4| L3] = re:split(L2, "#"),
    [_, Src] = re:split(L4, " client ", [{return, list}]),
    [_, _, _, Dst1] =  re:split(L3, ":", [{return, list}]),
%%%        [_, Dst1] =  re:split(L3, ":", [{return, list}]),
    [Dst] = string:tokens(Dst1, " "),
%    io:format("SRC = ~p, DST = ~p~n", [Src, Dst]), with the #value included
   {Src, Dst}.

%%%
% Reads benchmark network data and processes them
%%%

nci_from_benchmark_data(FileName) ->
    {ok, File} = file:open(FileName, [read]),
    {NumLines, NumProcessed, Links} = links_from_benchmark_data(File, 0, 0, []),
    file:close(File),
    NCI = nci:compute(Links),
    io:format("NumLines = ~p, NumProcessed = ~p NCI = ~p~n", [NumLines, NumProcessed, NCI]).

links_from_benchmark_data(File, NumLines, NumProcessed, Links) ->
    case io:get_line(File, "") of
        eof  -> {NumLines, NumProcessed, Links};
        Line ->            
            [V1, V2, _] = re:split(Line, "[\t\n]", [{return, list}]),
            links_from_benchmark_data(File, NumLines+1, NumProcessed+1, [{V1, V2}|Links])       
    end.
    



