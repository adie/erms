-module(erms_core_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% API Functions
start_link() ->
  supervisor:start_link(?MODULE, []).

init(_Args) ->
  {ok, {{one_for_one, 10, 60},
      [{erms_core, {erms_core, start, []},
          permanent, brutal_kill, worker, [erms_core]}]}}.

