-module(beat_core_app).

%% Application callbacks
-behaviour(application).
-export([
         start/2,
         stop/1
        ]).

%% ===================================================================
%%  Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    beat_core_sup:start_link().

stop(_State) ->
    ok.

%% ===================================================================
%%  Tests
%% ===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

callback_test_() ->
    [
     ?_assertMatch(ok, stop([]))
    ].

-endif.
