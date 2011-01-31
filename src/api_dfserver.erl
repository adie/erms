-module(api_dfserver).
-author("Anton Dieterle <antondie@gmail.com>").

-define(SNAME, {global, erms_df}).

-export([handle/5]).

handle('GET', _Request, [], _Args, _User) ->
  gen_server:call(?SNAME, do).

