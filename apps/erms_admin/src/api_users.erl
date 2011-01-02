%% @author Anton Dieterle <antondie@gmail.com>
%% @copyright 2010 Anton Dieterle <antondie@gmail.com>

%% @doc Users API resource for erms.

-module(api_users).
-author("Anton Dieterle <antondie@gmail.com>").

-define(SNAME, {global, erms_admin}).

-export([handle/5]).

-include("erms_admin.hrl").

handle(M, R, P, A, U) ->
  continue_if_admin(M,R,P,A,U).

handle(ok, 'GET', _Request, [], _Args, _User) ->
  gen_server:call(?SNAME, {list_users});
handle(ok, 'GET', _Request, [Id], _Args, _User) ->
  gen_server:call(?SNAME, {get_user, Id});
handle(ok, 'POST', _Request, [], Args, _User) ->
  gen_server:call(?SNAME, {create_user, Args});
handle(ok, 'POST', _Request, [Id], Args, _User) ->
  gen_server:call(?SNAME, {update_user, Id, Args});
handle(_,_,_,_,_,_) -> ok.

