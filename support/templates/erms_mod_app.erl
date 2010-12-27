%% @author {{author_name}}
%% @copyright 2010 {{author_name}}

%% @doc Callbacks for the {{name}} application

-module({{name}}_app).
-author("{{author_name}}").

-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
  {{name}}_sup:start_link().

stop(_State) ->
  ok.

