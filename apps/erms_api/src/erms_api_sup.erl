%% @author Anton Dieterle <antondie@gmail.com>
%% @copyright 2011 Anton Dieterle <antondie@gmail.com>

%% @doc erms_api supervisor.

-module(erms_api_sup).
-author("Anton Dieterle <antondie@gmail.com>").

-behaviour(supervisor).

%% External exports
-export([start_link/0, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
  {ok, {_, Specs}} = init([]),

  Old = sets:from_list(
    [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
  New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
  Kill = sets:subtract(Old, New),

  sets:fold(fun (Id, ok) ->
        supervisor:terminate_child(?MODULE, Id),
        supervisor:delete_child(?MODULE, Id),
        ok
    end, ok, Kill),

  [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
  ok.

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
  Web = web_specs(erms_api_web, 8080),

  Sessions = specs(erms_session_store),

  Processes = [Web, Sessions],

  Strategy = {one_for_one, 10, 10},
  {ok,
    {Strategy, lists:flatten(Processes)}}.

web_specs(Mod, Port) ->
    WebConfig = [{ip, {0,0,0,0}},
                 {port, Port},
                 {docroot, erms_deps:local_path(["priv", "www"])}],
    {Mod,
     {Mod, start, [WebConfig]},
     permanent, 5000, worker, dynamic}.

specs(Mod) ->
  {Mod,
    {Mod, start_link, []},
    permanent, 5000, worker, [Mod]}.

