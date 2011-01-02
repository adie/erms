%% @author Anton Dieterle <antondie@gmail.com>
%% @copyright 2010 Anton Dieterle <antondie@gmail.com>

%% @doc Folder's groups API resource for erms.

-module(api_folders_groups).
-author("Anton Dieterle <antondie@gmail.com>").

-define(SNAME, {global, erms_admin}).

-export([handle/5]).

-include("erms_admin.hrl").

handle(M, R, P, A, U) ->
  continue_if_admin(M,R,P,A,U).

handle(ok, 'GET', _Request, [], _Args, _User) ->
  gen_server:call(?SNAME, {list_folder_groups});
handle(ok, 'POST', _Request, [], Args, _User) ->
  gen_server:call(?SNAME, {create_folder_groups, Args});
handle(ok, 'DELETE', _Request, [Id], _Args, _User) ->
  gen_server:call(?SNAME, {delete_folder_groups, Id});
handle(_,_,_,_,_,_) -> ok.

