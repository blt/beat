-module(beat_core).

-export([
         register/1,
         unregister/1
        ]).

-export_type([]).

%% ===================================================================
%%  API
%% ===================================================================

-spec register(pid()) -> ok.
register(Pid) when is_pid(Pid) ->
    beat_core_beater:register(Pid).

-spec unregister(pid()) -> ok | {error, not_registered}.
unregister(Pid) when is_pid(Pid) ->
    beat_core_beater:unregister(Pid).

%% ===================================================================
%%  Tests
%% ===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

integration_test_() ->
    {setup,
     fun()  -> _ = application:start(beat_core) end,
     fun(_) -> _ = application:start(beat_core) end,
     [
      { "registration and deregistration",
        [
         ?_assertMatch(ok, beat_core:register(self())),
         ?_assertMatch(ok, beat_core:unregister(self()))
        ]
      }
     ]
    }.

-endif.
