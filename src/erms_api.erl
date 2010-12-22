%% @author Anton Dieterle <antondie@gmail.com>
%% @copyright 2010 Anton Dieterle <antondie@gmail.com>

%% @doc API handling for erms.

-module(erms_api).
-author("Anton Dieterle <antondie@gmail.com>").

-export([process_api/3]).

-record(context, {req, resp}).

%% Processing API requests

process_api(Request, Response, Params) ->
  Response1 = process(#context{req=Request, resp=Response}, Params),
  Response1:build_response().

process(#context{req=Request,resp=Response}, ["login",Login,Password]) ->
  case erms_auth:check_password(
      list_to_binary(Login),
      list_to_binary(Password)) of
    false ->
      Log = actions_log:new(
        calendar:universal_time(),
        get_ip(Request), 0, "",
        Request:uri(),
        Request:post_params(),
        failure,
        "Log in failure"
      ),
      actions_log:save(Log),
      return_error(Response, <<"Wrong username or password">>);
    User ->
      Log = actions_log:new(
        calendar:universal_time(),
        get_ip(Request), users:id(User), users:login(User),
        "",
        Request:post_params(),
        success,
        "Successfully logged in"
      ),
      actions_log:save(Log),
      SessionId = erms_session_store:create(users:id(User)),
      return_ok(Response, {struct, [
            {message, <<"You're logged in now">>},
            {session_id, list_to_binary(SessionId)}
          ]})
  end;

process(#context{req=Request,resp=Response}=Context, Path) ->
  Params = case
    Request:request_method() of
      'POST' -> Request:post_params();
      _ -> Request:query_params()
    end,
  case proplists:get_value("session_id", Params) of
    undefined ->
      return_error(Response, <<"You need to authenticate first">>);
    SessionId ->
      process(Context, Path, Params, SessionId)
  end.

process(#context{resp=Response}, ["logout"], _Params, SessionId) ->
  erms_session_store:destroy(SessionId),
  return_ok(Response, <<"You have logged out">>);

process(#context{req=Request,resp=Response}=Context, Path, Params, SessionId) ->
  case erms_session_store:read(SessionId) of
    not_found ->
      return_error(Response, <<"You need to authenticate first.">>);
    expired ->
      return_error(Response, <<"Your session expired. Plase authenticate again.">>);
    UserId ->
      User = users:find_id(UserId),
      Resp = process(Context, Request:request_method(), Path, Params, SessionId, User),
      {_,_,_,{response,200,_,_,{data, Data}}} = Resp,
      Log = actions_log:new(
        calendar:universal_time(),
        get_ip(Request), users:id(User), users:login(User),
        Request:path(),
        Request:request_body(),
        success,
        "" % Data
      ),
      actions_log:save(Log),
      Resp
  end.

process(Context, 'HEAD', Path, Params, SessionId, User) ->
  process(Context, 'GET', Path, Params, SessionId, User);
process(#context{req=Request, resp=Response}, Method, [Module,Function|Args], Params, _SessionId, User) ->
  M = list_to_atom(Module),
  F = list_to_atom(Function),
  case catch M:F(Method, Request, Args, Params, User) of
    {'EXIT', {Error, Desc}} ->
      error_logger:error_report([Error, Desc]),
      return_error(Response, [<<"Error in API call:">>, Error]);
    {error, Error} ->
      return_error(Response, [Error]);
    {response, ApiResponse} ->
      return_ok(Response, {struct, [{response, ApiResponse}]});
    {file, Filename, Binary} ->
      Resp1 = Response:header(
        "Content-Disposition",
        "attachment; filename=\""++binary_to_list(Filename)++"\""
      ),
      Resp1:data(Binary)
  end;
process(#context{resp=Response}, _Method, _Path, _Params, _SessionId, _User) ->
  return_error(Response, [<<"Requested resource does not exist">>]).

%% Response generation

return_ok(Response, {struct, _} = Json) ->
  Resp1 = Response:header("Content-Type", "text/json"),
  Resp1:data(mochijson2:encode(Json));
return_ok(Response, Message) ->
  return_ok(Response, {struct, [{message, Message}]}).

return_error(Response, Error) ->
  return_ok(Response, {struct, [{error, Error}]}).

get_ip(Request) ->
  {Q,W,E,R} = Request:peer_ip(),
  lists:concat([Q,".",W,".",E,".",R]).

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-endif.
