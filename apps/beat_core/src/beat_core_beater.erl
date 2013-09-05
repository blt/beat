-module(beat_core_beater).

-behaviour(gen_server).
-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
         ]).

-record(state, {timeout      = timer:seconds(1) :: pos_integer(),
                count        = 0                :: non_neg_integer(),
                linked_procs = []               :: [{pid(), reference()}]}).

-export([
         start_link/0,
         register/1,
         unregister/1
        ]).

%% ===================================================================
%%  API
%% ===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

register(Pid) when is_pid(Pid) ->
    gen_server:cast(?MODULE, {subscribe, Pid}).

unregister(Pid) when is_pid(Pid) ->
    gen_server:cast(?MODULE, {unsubscribe, Pid}).

%% ===================================================================
%%  gen_server callbacks
%% ===================================================================

init([]) ->
    S = #state{},
    ok = pulse(S#state.timeout),
    {ok, S}.

handle_call(_Request, _From, #state{}=S) ->
    {reply, ok, S}.

handle_cast(beat, #state{timeout=Timeout, count=C, linked_procs=LP}=S) ->
    Pids = [ Pid || {Pid, _} <- LP ],
    ok = pulse(Timeout),
    ok = lists:foreach(fun(P) -> gen_server:cast(P, {beat, C}) end, Pids),
    {noreply, S#state{count=C+1}};
handle_cast({subscribe, Pid}, #state{linked_procs=LP}=S) ->
    case proplists:get_value(Pid, LP) of
        undefined ->
            MonRef = erlang:monitor(process, Pid),
            {noreply, S#state{linked_procs=[{Pid, MonRef} | LP]}};
        MonRef when is_reference(MonRef) ->
            {noreply, S}
    end;
handle_cast({unsubscribe, Pid}, #state{linked_procs=LP}=S) ->
    case proplists:get_value(Pid, LP) of
        undefined ->
            {noreply, S};
        MonRef when is_reference(MonRef) ->
            _DidFlush = erlang:demonitor(MonRef, [flush]),
            LPs = proplists:delete(Pid, S#state.linked_procs),
            {noreply, S#state{linked_procs=LPs}}
    end.

handle_info({'DOWN', MonRef, process, Pid, _Info},
            #state{linked_procs=LP}=S) ->
    case proplists:get_value(Pid, LP) of
        undefined ->
            {noreply, S};
        MonRef ->
            true = erlang:demonitor(MonRef, [flush]),
            LPs = proplists:delete(Pid, S#state.linked_procs),
            {noreply, S#state{linked_procs=LPs}}
    end.

terminate(_Reason, #state{}=_S) ->
    ok.

code_change(_OldVsn, #state{}=_S, _Extra) ->
    {error, not_implemented}.

%% ===================================================================
%%  Internal Functions
%% ===================================================================

-spec pulse(non_neg_integer()) -> ok.
pulse(Timeout) when is_integer(Timeout), Timeout > 0 ->
    {ok, _} = timer:apply_after(Timeout, gen_server, cast, [?MODULE, beat]),
    ok.

%% ===================================================================
%%  Tests
%% ===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

protocol_test_() ->
    Self = self(),
    MonRef = erlang:monitor(process, Self),

    S  = #state{},
    LP = #state{linked_procs=[{Self, MonRef}]},

    [
     { "beats",
       [
        ?_assertMatch({noreply, #state{}},
                      handle_cast(beat, S))
       ]
     },
     { "registration",
       [
        ?_assertMatch({noreply, #state{linked_procs=[{Self, _}]}},
                      handle_cast({subscribe, Self}, S)),
        ?_assertMatch({noreply, LP},
                      handle_cast({subscribe, Self}, LP)),

        ?_assertMatch({noreply, S},
                      handle_cast({unsubscribe, Self}, S)),
        ?_assertMatch({noreply, S},
                      handle_cast({unsubscribe, Self}, LP))
       ]
     },
     { "down messages",
       [
        ?_assertMatch({noreply, S},
                      handle_info({'DOWN', MonRef, process, Self, info}, S)),
        ?_assertMatch({noreply, S},
                      handle_info({'DOWN', MonRef, process, Self, info}, LP))
       ]
     }
    ].

callback_test_() ->
    S = #state{},

    [
     ?_assertMatch({ok, S},                  init([])),
     ?_assertMatch({reply, ok, S},           handle_call(request, from, S)),
     ?_assertMatch(ok,                       terminate(reason, S)),
     ?_assertMatch({error, not_implemented}, code_change(old_vsn, S, extra))
    ].

-endif.
