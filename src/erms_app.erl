%% @author Mochi Media <dev@mochimedia.com>
%% @copyright erms Mochi Media <dev@mochimedia.com>

%% @doc Callbacks for the erms application.

-module(erms_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).

%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for erms.
start(_Type, _StartArgs) ->
    erms_deps:ensure(),
    erms_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for erms.
stop(_State) ->
    ok.

