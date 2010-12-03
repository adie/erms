-module(erms_core_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
  erms_core_sup:start_link().

stop(_State) ->
  ok.
