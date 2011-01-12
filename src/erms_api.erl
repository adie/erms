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

% Process login request
process(#context{req=Request,resp=Response}, ["login"]) ->
  Login = proplists:get_value("username", Request:post_params()),
  Password = proplists:get_value("password", Request:post_params()),
  case erms_auth:check_password(
      list_to_binary(Login),
      list_to_binary(Password)) of
    false ->
      erms_log:log({login_failure, Request}),
      return_error(Response, <<"Wrong username or password">>);
    User ->
      erms_log:log({login_success, Request, User}),
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

% Process logout request
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
      erms_log:log({request, Request, User}),
      Resp = process(Context, Request:request_method(), Path, Params, SessionId, User),
      erms_log:log({response, Request, User, Resp}),
      Resp
  end.

% Execute requested operation
process(#context{req=Request, resp=Response}, Method, [Resource|Args], Params, _SessionId, User) ->
  R = list_to_atom("api_"++Resource),
  case catch R:handle(Method, Request, Args, Params, User) of
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

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-endif.
