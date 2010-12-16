%% @author Anton Dieterle <antondie@gmail.com>
%% @copyright 2010 Anton Dieterle <antondie@gmail.com>

%% @doc API handling for erms.

-module(erms_api).
-author("Anton Dieterle <antondie@gmail.com>").

-export([process_api/4]).

-record(context, {req, doc_root, simple_req}).

%% Processing API requests

process_api(Req, DocRoot, SimpleBridge, Params) ->
  process(#context{req=Req, doc_root=DocRoot, simple_req=SimpleBridge}, Params).

process(#context{req=Req}, ["login",Login,Password]) ->
  case erms_auth:check_password(
      list_to_binary(Login),
      list_to_binary(Password)) of
    false ->
      return_error(Req, <<"Wrong username or password">>);
    User ->
      SessionId = erms_session_store:create(users:id(User)),
      return_ok(Req, {struct, [
            {message, <<"You're logged in now">>},
            {session_id, list_to_binary(SessionId)}
          ]})
  end;

process(#context{req=Req,simple_req=Request}=Context, Path) ->
  Params = case
    Request:request_method() of
      'POST' -> Request:post_params();
      _ -> Request:query_params()
    end,
  case proplists:get_value("session_id", Params) of
    undefined ->
      return_error(Req, <<"You need to authenticate first">>);
    SessionId ->
      process(Context, Path, Params, SessionId)
  end.

process(#context{req=Req}, ["logout"], _Params, SessionId) ->
  erms_session_store:destroy(SessionId),
  return_ok(Req, <<"You have logged out">>);

process(#context{req=Req}=Context, Path, Params, SessionId) ->
  case erms_session_store:read(SessionId) of
    not_found ->
      return_error(Req, <<"You need to authenticate first.">>);
    expired ->
      return_error(Req, <<"Your session expired. Plase authenticate again.">>);
    UserId ->
      process(Context, Req:get(method), Path, Params, SessionId, users:find_id(UserId))
  end.

process(Context, 'HEAD', Path, Params, SessionId, User) ->
  process(Context, 'GET', Path, Params, SessionId, User);
process(#context{req=Req,simple_req=Request}, Method, [Module,Function|Args], Params, _SessionId, User) ->
  M = list_to_atom(Module),
  F = list_to_atom(Function),
  case catch M:F(Method, Request, Args, Params, User) of
    {'EXIT', {Error, Desc}} ->
      error_logger:error_report([Error, Desc]),
      return_error(Req, [<<"Error in API call:">>, Error]);
    {error, Error} ->
      return_error(Req, [Error]);
    {response, Response} ->
      return_ok(Req, {struct, [{response, Response}]});
    {file, Filename, Binary} ->
      Req:ok({
          "",
          [{"Content-Disposition",
              "attachment; filename=\""++binary_to_list(Filename)++"\""}],
          Binary
        })
  end;
process(#context{req=Req}, _Method, _Path, _Params, _SessionId, _User) ->
  return_error(Req, [<<"Requested resource does not exist">>]).

%% Response generation

return_ok(Req, Json) ->
  return_ok(Req, [], Json).

return_ok(Req, Headers, {struct, _} = Json) ->
  Req:ok({"text/json", Headers, mochijson2:encode(Json) });
return_ok(Req, Headers, Message) ->
  return_ok(Req, Headers, {struct, [{message, Message}]}).

return_error(Req, Error) ->
  return_ok(Req, {struct, [{error, Error}]}).

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-endif.
