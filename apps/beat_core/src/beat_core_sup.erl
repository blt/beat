-module(beat_core_sup).

%% API
-export([
         start_link/0
        ]).

%% Supervisor callbacks
-behaviour(supervisor).
-export([
         init/1
        ]).

-define(SCOPE, local).

%% ===================================================================
%%  API
%% ===================================================================

start_link() ->
    supervisor:start_link({?SCOPE, ?MODULE}, ?MODULE, []).

%% ===================================================================
%%  Supervisor callbacks
%% ===================================================================

init([]) ->
    BeaterSpec = {bc_beater,
                  {beat_core_beater, start_link, []},
                  permanent,
                  5000,
                  worker,
                  [beat_core_beater]},
    {ok, { {one_for_one, 5, 10}, [BeaterSpec]}}.

%% ===================================================================
%%  Tests
%% ===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

sanity_test_() ->
    [
     ?_assertMatch(local, ?SCOPE)
    ].

callback_test_() ->
    [
     ?_assertMatch({ok, {{one_for_one, _, _}, [_]}}, init([]))
    ].

-endif.
