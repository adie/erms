%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc erms.

-module(erms).
-author("Mochi Media <dev@mochimedia.com>").
-export([start/0, stop/0]).
-export([ensure_started/1]).

ensure_started([]) ->
  ok;
ensure_started([App|Modules]) ->
  ensure_started(App),
  ensure_started(Modules);
ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% @spec start() -> ok
%% @doc Start the erms server.
start() ->
    erms_deps:ensure(),

    application:start(erms),

    {ok, Modules} = application:get_env(erms, modules),
    ensure_started(Modules).

%% @spec stop() -> ok
%% @doc Stop the erms server.
stop() ->
    application:stop(erms).

