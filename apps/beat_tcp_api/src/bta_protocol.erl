-module(bta_protocol).

-behaviour(gen_server).
-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
         ]).

-behaviour(ranch_protocol).
-export([
         start_link/4,
         init/4
        ]).

-record(state, {
          ref :: any(),
          socket :: inet:socket(),
          transport :: module(),
          beat_core :: module()
         }).

-ifdef(TEST).
-export([
         send/2,
         setopts/2,
         register/1
        ]).
-endif.

%% ===================================================================
%%  ranch_protocol callbacks
%% ===================================================================

start_link(Ref, Socket, Transport, Opts) ->
    proc_lib:start_link(?MODULE, init, [Ref, Socket, Transport, Opts]).

init(Ref, Socket, Transport, _Opts = []) ->
    ok = proc_lib:init_ack({ok, self()}),
    ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket, [{active, once}]),
    State = #state{ref=Ref, socket=Socket,
                   transport=Transport, beat_core=beat_core},
    gen_server:enter_loop(?MODULE, [], State, 0).

%% ===================================================================
%%  gen_server callbacks
%% ===================================================================

init([]) ->
    {ok, #state{}}.

handle_call(_Request, _From, #state{}=S) ->
    {reply, ok, S}.

handle_cast({beat, I}, #state{socket=Sock, transport=Trans}=S) when is_integer(I) ->
    Trans:send(Sock, io_lib:format("~p~n", [I])),
    {noreply, S}.

handle_info(timeout, #state{beat_core=BC}=S) ->
    ok = BC:register(self()),
    {noreply, S};
handle_info({tcp, _Port, Msg}, #state{socket=Sock, transport=Trans}=S) ->
    ok = Trans:send(Sock, Msg),
    ok = Trans:setopts(Sock, [{active, once}]),
    {noreply, S};
handle_info({tcp_closed, _Port}, #state{}=S) ->
    {noreply, S}.

terminate(_Reason, #state{}=_S) ->
    ok.

code_change(_OldVsn, #state{}=_S, _Extra) ->
    {error, not_implemented}.

%% ===================================================================
%%  Internal Functions
%% ===================================================================

%% ===================================================================
%%  Tests
%% ===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-define(TESTMSG, <<"beat\n">>).
-define(TESTSOCK, socksock).

send(?TESTSOCK, ?TESTMSG) ->
    ok;
send(?TESTSOCK, ["10", "\n"]) ->
    ok.
setopts(?TESTSOCK, [{active, once}]) ->
    ok.
register(Pid) ->
    Pid = self(),
    ok.

interaction_test_() ->
    S = #state{beat_core=?MODULE, socket=?TESTSOCK, transport=?MODULE},

    [
     ?_assertMatch({noreply, S}, handle_cast({beat, 10}, S)),
     ?_assertMatch({noreply, S}, handle_info(timeout, S)),
     ?_assertMatch({noreply, S}, handle_info({tcp, port, ?TESTMSG}, S)),
     ?_assertMatch({noreply, S}, handle_info({tcp_closed, port}, S))
    ].

callback_test_() ->
    S = #state{},

    [
     ?_assertMatch({ok, #state{}},           init([])),
     ?_assertMatch({reply, ok, S},           handle_call(request, from, S)),
     ?_assertMatch({noreply, S},             handle_info({tcp_closed, port}, S)),
     ?_assertMatch(ok,                       terminate(reason, S)),
     ?_assertMatch({error, not_implemented}, code_change(old_vsn, S, extra))
    ].

-endif.
