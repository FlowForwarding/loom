%%%-------------------------------------------------------------------
%%% @copyright (C) 1999-2013, Erlang Solutions Ltd
%%% @author Marc Sugiyama <marc.sugiyama@erlang-solutions.com>
%%% @doc
%%% Simple network executive.
%%% @end
%%%-------------------------------------------------------------------
-module(simple_ne_app).
-copyright("2013, Erlang Solutions Ltd.").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    simple_ne_sup:start_link().

stop(_State) ->
    ok.
