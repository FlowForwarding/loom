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
%% @author Ryan Crum <ryan.j.crum@gmail.com>, Infoblox Inc <info@infoblox.com>
%% @copyright 2013 Infoblox Inc
%% @doc FTP service to connect Tapestry to the Infoblox Grid (6.9 or later).  The code is based on 
%% memory_server.erl example from Ryan Crum's bifrost Erlang based ftp server


-module(tap_batch).

-behavior(gen_server).

-export([start_link/0,
         load/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(STATE, tap_batch_state).
-record(?STATE, {
    }).

% -----------------------------------------------------------------------------
% API
% -----------------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

load(Data) ->
    gen_server:cast(?MODULE, {load, Data}).

% -----------------------------------------------------------------------------
% bifrost callbacks
% -----------------------------------------------------------------------------

init([]) ->
    {ok, #?STATE{}}.

handle_call(Msg, From, State) ->
    error({no_handle_call, ?MODULE}, [Msg, From, State]).

handle_cast({load, FtpFile}, State) ->
    BinaryFile = extract_file(FtpFile),
    Data = parse_file(BinaryFile),
    error_logger:info_msg("Data = ~p~n",[Data]),
    tap_ds:ordered_edges(Data),
    {noreply, State};
handle_cast(Msg, State) ->
    error({no_handle_cast, ?MODULE}, [Msg, State]).

handle_info(Msg, State) ->
    error({no_handle_info, ?MODULE}, [Msg, State]).

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

% -----------------------------------------------------------------------------
% private functions
% -----------------------------------------------------------------------------

extract_file(CompressedTarBytes)->
    % XXX look for .log?
    {ok, [{_, BinaryFile}, _, _, _]} =
        erl_tar:extract({binary, CompressedTarBytes}, [compressed, memory]),
    BinaryFile.

parse_file(BinaryData) ->
    parse_file(BinaryData, []).

parse_file(<<BitString:53/binary, BinaryData/binary>>, Data) ->
    <<_Time:10/binary, _S:1/binary,
       ID1:20/binary, _S:1/binary,
       ID2:20/binary, _Rest/binary>> = BitString,
    V1 = bitstring_to_list(ID1),
    V2 = bitstring_to_list(ID2),
    Interaction = {V1, V2},
    parse_file(BinaryData, [Interaction | Data]);
parse_file(_BinaryData, Data)->
    lists:reverse(Data).
