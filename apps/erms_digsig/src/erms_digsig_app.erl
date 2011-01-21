%% @author Anton Dieterle <antondie@gmail.com>
%% @copyright 2011 Anton Dieterle <antondie@gmail.com>

%% @doc Callbacks for the erms_digsig application

-module(erms_digsig_app).
-author("Anton Dieterle <antondie@gmail.com>").

-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
  erms:ensure_started([crypto, public_key]),
  erms_digsig_sup:start_link().

stop(_State) ->
  ok.

