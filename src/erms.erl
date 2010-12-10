%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc erms.

-module(erms).
-author("Mochi Media <dev@mochimedia.com>").
-export([start/0, stop/0]).

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
    ensure_started(crypto),
    ensure_started(erms_db),
    application:start(erms).

%% @spec stop() -> ok
%% @doc Stop the erms server.
stop() ->
    application:stop(erms).
