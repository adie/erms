%% @author Anton Dieterle <antondie@gmail.com>
%% @copyright 2010 Anton Dieterle <antondie@gmail.com>

%% @doc API handling for erms.

-module(erms_api).
-author("Anton Dieterle <antondie@gmail.com>").

-export([process_api/2]).

%% Processing API requests

process_api(Req, ["login",Login,Password]) ->
  case erms_auth:check_password(
      list_to_binary(Login),
      list_to_binary(Password)) of
    true ->
      User = users:find_first({login, '=', Login}),
      SessionId = erms_session_store:create(users:id(User)),
      return_ok(Req, {struct, [
            {message, <<"You're logged in now">>},
            {session_id, list_to_binary(SessionId)}
          ]});
    _ ->
      return_error(Req, <<"Wrong username or password">>)
  end;

process_api(Req, Params) ->
  case proplists:get_value("session_id", Req:parse_qs()) of
    undefined ->
      return_error(Req, <<"You need to authenticate first">>);
    SessionId ->
      process_api(Req, Params, SessionId)
  end.

process_api(Req, ["logout"], SessionId) ->
  erms_session_store:destroy(SessionId),
  return_ok(Req, <<"You have logged out">>);

process_api(Req, Params, SessionId) ->
  io:format("SessionId: ~p~n", [SessionId]),
  case erms_session_store:read(SessionId) of
    not_found ->
      return_error(Req, <<"You need to authenticate first.">>);
    expired ->
      return_error(Req, <<"Your session expired. Plase authenticate again.">>);
    UserId ->
      io:format("UserId: ~p~n", [UserId]),
      process_api(Req, Params, SessionId, users:find_id(UserId))
  end.

process_api(Req, [Module,Function|Args], SessionId, User) ->
  M = list_to_atom(Module),
  F = list_to_atom(Function),
  case catch M:F(Args) of
    {'EXIT', {Error, _}} ->
      return_error(Req, [<<"Error in API call:">>, Error]);
    Result ->
      return_ok(Req, {struct, [{result, Result}]})
  end;

process_api(Req, [Module], SessionId, User) ->
  M = list_to_atom(Module),
  case (catch M:api_functions()) of
    {'EXIT', {undef, _}} ->
      return_error(Req, <<"No such module">>);
    Funs ->
      return_ok(Req, {struct, [{functions, Funs}]})
  end;

process_api(Req, _Params, _SessionId, User) ->
  return_ok(Req, [<<"Welcome to the API">>, users:fullname(User)]).

%% Response generation

return_ok(Req, Json) ->
  return_ok(Req, [], Json).

return_ok(Req, Headers, {struct, _} = Json) ->
  Req:ok({"text/plain", Headers, mochijson2:encode(Json) });
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
