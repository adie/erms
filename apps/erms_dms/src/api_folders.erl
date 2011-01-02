%% @author Anton Dieterle <antondie@gmail.com>
%% @copyright 2010 Anton Dieterle <antondie@gmail.com>

%% @doc Document folder API resource for erms.

-module(api_folders).
-author("Anton Dieterle <antondie@gmail.com>").

-define(SNAME, {global, erms_dms}).

-export([handle/5]).

handle('GET', Request, [], Args, User) ->
  handle('GET', Request, [0], Args, User);
handle('GET', _Request, [Id], _Args, User) ->
  gen_server:call(?SNAME, {get_folder, Id, User});
handle('POST', Request, [], Args, User) ->
  handle('POST', Request, [0], Args, User);
handle('POST', _Request, [Id], Args, User) ->
  gen_server:call(?SNAME, {create_folder, Id, Args, User});
handle(_,_,_,_,_) -> ok.

