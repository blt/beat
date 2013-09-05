-module(beat_tcp_api_sup).

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
    RanchSupSpec = {ranch_sup, {ranch_sup, start_link, []},
                    permanent, 5000, supervisor, [ranch_sup]},
    ListenerSpec = ranch:child_spec(beat_tcp, 100,
                                    ranch_tcp, [{port, 27182}],
                                    bta_protocol, []),
    {ok, { {one_for_one, 5, 10}, [
                                  RanchSupSpec,
                                  ListenerSpec
                                 ]} }.

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
     ?_assertMatch({ok, {{one_for_one, _, _}, [_, _]}}, init([]))
    ].

-endif.
