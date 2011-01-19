%% @author Anton Dieterle <antondie@gmail.com>
%% @copyright 2011 Anton Dieterle <antondie@gmail.com>

%% @doc Callbacks for the erms_api application

-module(erms_api_app).
-author("Anton Dieterle <antondie@gmail.com>").

-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
  erms_api_sup:start_link().

stop(_State) ->
  ok.

