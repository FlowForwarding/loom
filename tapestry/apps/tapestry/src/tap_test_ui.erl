-module(tap_test_ui).

% @doc
% Generates random sample data to test ui.  Enable with sys.config
% parameter {test_ui, enabled}.
% @end

-behavior(gen_server).

-export([start_link/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(STATE, tap_test_ui_state).
-record(?STATE, {}).

%------------------------------------------------------------------------------
% API
%------------------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%------------------------------------------------------------------------------
% gen_server callbacks
%------------------------------------------------------------------------------

init([]) ->
    {ok, #?STATE{}}.

handle_call(Msg, From, State) ->
    error({no_handle_call, ?MODULE}, [Msg, From, State]).

handle_cast({newclient, Pid}, State) ->
    ok = start_feeds(Pid),
    {noreply, State};
handle_cast(Msg, State) ->
    error({no_handle_cast, ?MODULE}, [Msg, State]).

handle_info(Msg, State) ->
    error({no_handle_info, ?MODULE}, [Msg, State]).

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

%------------------------------------------------------------------------------
% Local Functions
%------------------------------------------------------------------------------

start_feeds(Pid) ->
    start_feed(Pid, fun nci/0),
    start_feed(Pid, fun qps/0),
    start_feed(Pid, fun nep/0),
    ok.

start_feed(Pid, Feed) ->
    spawn(fun() ->
            random:seed(now()),
            monitor(process, Pid),
            feed_loop(Pid, Feed)
          end).

feed_loop(Pid, Feed) ->
    {Msg, Wait} = Feed(),
    clientsock:send(Pid, Msg),
    receive
        {'DOWN', _, process, Pid, _} ->
            % exit when the websocket client disconnects
            stop
    after
        Wait ->
            feed_loop(Pid, Feed)
    end.

nci()->
    Wait = random:uniform(4) * 500,
    NCI = random:uniform(100),
    Time = list_to_binary(tap_utils:rfc3339(erlang:universaltime())),
    JSON = jiffy:encode({[{<<"Time">>,Time},{<<"NCI">>,NCI}]}),
    {JSON, Wait}.

qps()->
    Wait = random:uniform(2) * 500,
    QPS = random:uniform(2000000) + 1000000,
    Time = list_to_binary(tap_utils:rfc3339(erlang:universaltime())),
    JSON = jiffy:encode({[{<<"Time">>,Time},{<<"QPS">>,QPS}]}),
    {JSON, Wait}.

nep()->
    Wait = random:uniform(10) * 500,
    NEP = random:uniform(50000) + 200000,
    Time = list_to_binary(tap_utils:rfc3339(erlang:universaltime())),
    JSON = jiffy:encode({[{<<"Time">>,Time},{<<"NEP">>,NEP}]}),
    {JSON, Wait}.
