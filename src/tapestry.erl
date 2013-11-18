%%------------------------------------------------------------------------------
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%%-----------------------------------------------------------------------------
%%
%% @author Infoblox Inc <info@infoblox.com>
%% @copyright 2013 Infoblox Inc
%% @doc Tapestry.  A Network Complexity Analyzer

-module(tapestry).


-compile([all]).

-export([start/0,start/1,nci_from_log_lines/1,nci_from_benchmark_data/1]).

start()->
    start(loom).

start(Type)->
    case Type of
	loom ->
	    start_loom();
	grid  ->
	    start_grid()
    end.
	   

start_loom()->
    error_logger:info_msg("Starting tapestry. Loom Mode. View $TAPESTRY_HOME/log/console.log for operational messages.~n"),
    [code:add_pathz(Path) || Path <- filelib:wildcard("./lib/loom/ebin")],
    [code:add_pathz(Path) || Path <- filelib:wildcard("./lib/loom/deps/*/ebin")],
    [code:add_pathz(Path) || Path <- filelib:wildcard("./lib/loom/apps/*/ebin")],
    loom_app:start(),
    loom_sup:launch_controller(dns_tap,6634),
    tap_yaws:start(),
    Pid = tap_aggr:start(),
    error_logger:info_msg("Stared tapestry with Process ID ~p.~n",[Pid]),
    Pid.

start_grid()->
    {ftpd,Port} = tap_config:get([ports,ftpd]),
    error_logger:info_msg("Starting tapestry.~n Grid Mode. Listening via FTP on port ~p.~n View $TAPESTRY_HOME/log/console.log for operational messages.~n",[Port]),
    [code:add_pathz(Path) || Path <- filelib:wildcard("./lib/loom/ebin")],
    [code:add_pathz(Path) || Path <- filelib:wildcard("./lib/loom/deps/*/ebin")],
    [code:add_pathz(Path) || Path <- filelib:wildcard("./lib/loom/apps/*/ebin")],
    [code:add_pathz(Path) || Path <- filelib:wildcard("./lib/bifrost/ebin")],
    [code:add_pathz(Path) || Path <- filelib:wildcard("./lib/bifrost/deps/*/ebin")],
    loom_app:start(),
    tap_yaws:start(),
    Pid = tap_ftpd:start(),
    error_logger:info_msg("Stared tapestry with Process ID ~p.~n",[Pid]),
    Pid.





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
    



