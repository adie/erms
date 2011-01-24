%% @author Anton Dieterle <antondie@gmail.com>
%% @copyright 2011 Anton Dieterle <antondie@gmail.com>

%% @doc Callbacks for the erms_crypto application

-module(erms_crypto_app).
-author("Anton Dieterle <antondie@gmail.com>").

-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
  erms:ensure_started([crypto, public_key]),
  erms_crypto_sup:start_link().

stop(_State) ->
  ok.

