%% @author Anton Dieterle <antondie@gmail.com>
%% @copyright 2010 Anton Dieterle <antondie@gmail.com>

%% @doc API handling for erms.

-module(erms_api).
-author("Anton Dieterle <antondie@gmail.com>").

-export([process_api/2, login_cookies/1]).

%% Processing API requests

process_api(Req, ["login",Login,Password]) ->
  case erms_auth:check_password(
      list_to_binary(Login),
      list_to_binary(Password)) of
    true ->
      User = users:find_first({login, '=', Login}),
      api_ok(Req, login_cookies(users:id(User)), <<"You're logged in now">>);
    _ ->
      api_error(Req, <<"Wrong username or password">>)
  end;

process_api(Req, ["logout"]) ->
  api_ok(Req, login_cookies("", ""), <<"You have logged out">>);

process_api(Req, Params) ->
  case Req:get_cookie_value("user_id") of
    undefined ->
      api_error(Req, <<"You need to authenticate first">>);
    UserId ->
      case users:find_first({id, '=', UserId}) of
        undefined ->
          api_error(Req, <<"You need to authenticate first">>);
        User ->
          case erms_session_store:read(Req:get_cookie_value("session_id")) of
            not_found ->
              api_error(Req, <<"You need to authenticate first.">>);
            expired ->
              api_error(Req, <<"Your session expired. Plase authenticate again.">>);
            SessUserId ->
              process_api(Req, Params, User)
          end
      end
  end.

process_api(Req, [Module,Function|Args], User) ->
  M = list_to_atom(Module),
  F = list_to_atom(Function),
  case catch M:F(Args) of
    {'EXIT', {Error, _}} ->
      api_error(Req, [<<"Error in API call:">>, Error]);
    Result ->
      api_ok(Req, {struct, [{result, Result}]})
  end;

process_api(Req, [Module], User) ->
  M = list_to_atom(Module),
  case (catch M:api_functions()) of
    {'EXIT', {undef, _}} ->
      api_error(Req, <<"No such module">>);
    Funs ->
      api_ok(Req, {struct, [{functions, Funs}]})
  end;

process_api(Req, _Params, User) ->
  api_ok(Req, [<<"Welcome to the API">>, users:fullname(User)]).

%% Response generation

api_ok(Req, Json) ->
  api_ok(Req, [], Json).

api_ok(Req, Headers, Json) ->
  Req:ok({"text/plain", Headers, mochijson2:encode(Json) }).

api_error(Req, Error) ->
  api_ok(Req, {struct, [{error, Error}]}).

%% Using login information

login_cookies(UserId) ->
  login_cookies(UserId, erms_session_store:create(UserId)).
login_cookies(UserId, SessionId) ->
  [ mochiweb_cookies:cookie("user_id", UserId, [{path, "/"}]),
    mochiweb_cookies:cookie("session_id", SessionId, [{path, "/"}]) ].

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-endif.
