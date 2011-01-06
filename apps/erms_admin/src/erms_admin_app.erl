%% @author Anton Dieterle <antondie@gmail.com>
%% @copyright 2010 Anton Dieterle <antondie@gmail.com>

%% @doc Callbacks for the erms admin application.

-module(erms_admin_app).
-author("Anton Dieterle <antondie@gmail.com>").

-behaviour(application).
-export([start/2, stop/1]).
-export([start/0, stop/0]).

start(_Type, _StartArgs) ->
  erms_admin_sup:start_link().

stop(_State) ->
  ok.

start() ->
  application:start(erms_admin).

stop() ->
  application:stop(erms_admin).

