-module(beat_tcp_api).

-export([
        ]).

-export_type([]).

%% ===================================================================
%%  API
%% ===================================================================

%% ===================================================================
%%  Tests
%% ===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

integration_test_() ->
    {setup,
     fun()  -> _ = application:start(beat_tcp_api) end,
     fun(_) -> _ = application:start(beat_tcp_api) end,
     [
     ]
    }.

-endif.
