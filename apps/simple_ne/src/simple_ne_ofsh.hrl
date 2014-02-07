%%%-------------------------------------------------------------------
%%% @copyright (C) 1999-2013, Erlang Solutions Ltd
%%% @author Marc Sugiyama <marc.sugiyama@erlang-solutions.com>
%%% @doc
%%% Simple network executive callback handler for ofs_handler.
%%% @end
%%%-------------------------------------------------------------------
% State held by ofs_handler.
% This state holds onto the datapath id and aux connection id.
% There is one state for each connection.  
-define(OFS_STATE, simple_ne_ofs_state).
-record(?OFS_STATE, {
                    datapath_id,
                    aux_id = 0
                }).
-type ofs_state() :: #?OFS_STATE{}.
