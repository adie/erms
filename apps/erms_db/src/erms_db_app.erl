%% @author Anton Dieterle <antondie@gmail.com>
%% @copyright 2010 Anton Dieterle <antondie@gmail.com>

%% @doc Callbacks for the erms database application.

-module(erms_db_app).
-author("Anton Dieterle <antondie@gmail.com>").

-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
  erms_deps:ensure(),
  erms_db_sup:start_link().

stop(_State) ->
  ok.

