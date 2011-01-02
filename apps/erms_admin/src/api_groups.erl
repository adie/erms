%% @author Anton Dieterle <antondie@gmail.com>
%% @copyright 2010 Anton Dieterle <antondie@gmail.com>

%% @doc Groups API resource for erms.

-module(api_groups).
-author("Anton Dieterle <antondie@gmail.com>").

-define(SNAME, {global, erms_admin}).

-export([handle/5]).

-include("erms_admin.hrl").

handle(M, R, P, A, U) ->
  continue_if_admin(M,R,P,A,U).

handle(ok, 'GET', _Request, [], _Args, _User) ->
  gen_server:call(?SNAME, {list_groups});
handle(ok, 'GET', _Request, [Id], _Args, _User) ->
  gen_server:call(?SNAME, {get_group, Id});
handle(ok, 'POST', _Request, [], Args, _User) ->
  gen_server:call(?SNAME, {create_group, Args});
handle(ok, 'POST', _Request, [Id], Args, _User) ->
  gen_server:call(?SNAME, {update_group, Id, Args});
handle(_,_,_,_,_,_) -> ok.

