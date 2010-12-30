%% @author Anton Dieterle <antondie@gmail.com>
%% @copyright 2010 Anton Dieterle <antondie@gmail.com>

%% @doc Document API resource for erms.

-module(api_documents).
-author("Anton Dieterle <antondie@gmail.com>").

-define(SNAME, {global, erms_dms}).

-export([handle/5]).

handle('GET', _Request, [Id], _Args, _User) ->
  gen_server:call(?SNAME, {get_document, Id});
handle('POST', Request, [], Args, User) ->
  [File|_] = Request:post_files(),
  gen_server:call(?SNAME, {create_document, Args, File, User});
handle(_,_,_,_,_) -> ok.

