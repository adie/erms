%% @author Anton Dieterle <antondie@gmail.com>
%% @copyright 2010 Anton Dieterle <antondie@gmail.com>

%% @doc Web server for erms.

-module(erms_web).
-author("Anton Dieterle <antondie@gmail.com>").

-export([start/1, stop/0, loop/2]).

%% External API

start(Options) ->
  {DocRoot, Options1} = get_option(docroot, Options),
  Loop = fun (Req) ->
      ?MODULE:loop(Req, DocRoot)
  end,
  mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
  mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
  Path = Req:get(path),
  try
    case string:tokens(Path, "/") of
      ["api"|Params] ->
        process_api(Req, Params);
      _ ->
        % Process non-API requests
        case Req:get(method) of
          Method when Method =:= 'GET'; Method =:= 'HEAD' ->
            case Path of
              "/" ->
                Req:ok({"text/html", [], render(Path, DocRoot, [
                        {messages, erms_core:get_all_msg()}
                      ])});
              _ ->
                Req:serve_file(Path, DocRoot)
            end;
          'POST' ->
            case Path of
              _ ->
                Req:not_found()
            end;
          _ ->
            Req:respond({501, [], []})
        end
    end
  catch
    Type:What ->
      Report = ["web request failed",
        {path, Path},
        {type, Type}, {what, What},
        {trace, erlang:get_stacktrace()}],
      error_logger:error_report(Report),
      Req:respond({500, [{"Content-Type", "text/plain"}],
          "request failed, sorry\n"})
end.

%% Internal API

process_api(Req, Params) ->
  case Params of
    ["login",Login,Password] ->
      case erms_auth:check_password(
          list_to_binary(Login),
          list_to_binary(Password)) of
        true ->
          User = users:find_first({login, '=', Login}),
          api_ok(Req, login_cookies(users:id(User)), <<"You're logged in now">>);
        _ ->
          api_error(Req, <<"Wrong username or password">>)
      end;
    ["logout"|_] ->
      api_ok(Req, login_cookies("", ""), <<"You have logged out">>);
    _ ->
      case Req:get_cookie_value("user_id") of
        undefined ->
          api_error(Req, <<"You need to authenticate first">>);
        UserId ->
          case users:find_first({id, '=', UserId}) of
            undefined ->
              api_error(Req, <<"You need to authenticate first">>);
            User ->
              case session_identifier(UserId) =:= Req:get_cookie_value("session_id") of
                false ->
                  api_error(Req, <<"You need to authenticate first">>);
                true ->
                  process_api(Req, Params, User)
              end
          end
      end
  end.

process_api(Req, Params, User) ->
  case Params of
    [Module,Function|Args] ->
      M = list_to_atom(Module),
      F = list_to_atom(Function),
      case catch M:F(Args) of
        {'EXIT', {Error, _}} ->
          api_error(Req, [<<"Error in API call:">>, Error]);
        Result ->
          api_ok(Req, {struct, [{result, Result}]})
      end;
    [Module] ->
      M = list_to_atom(Module),
      case (catch M:api_functions()) of
        {'EXIT', {undef, _}} ->
          api_error(Req, <<"No such module">>);
        Funs ->
          api_ok(Req, {struct, [{functions, Funs}]})
      end;
    _ ->
      api_ok(Req, [<<"Welcome to the API">>, users:fullname(User)])
  end.

api_ok(Req, Json) ->
  api_ok(Req, [], Json).
api_ok(Req, Headers, Json) ->
  Req:ok({"text/plain", Headers,
      mochijson2:encode(Json)
    }).
api_error(Req, Error) ->
  api_ok(Req, {struct, [{error, Error}]}).

render(Path, DocRoot, Data) ->
  case string:right(Path, 1) of
    "/" ->
      FinePath = filename:join([Path, "index.html"]);
    _ ->
      FinePath = Path
  end,
  "/" ++ FileName = FinePath,
  {ok, Rendered} = render_template(
    filename:join([DocRoot, mochiweb_util:safe_relative_path(FileName)]),
    Data
  ),
  Rendered.

render_template(FullPath, Data) ->
  erlydtl:compile(FullPath, template),
  template:render(Data).

get_option(Option, Options) ->
  {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

session_identifier(UserId) ->
  {ok, [{secret, Secret}]} = application:get_env(erms, auth),
  mochihex:to_hex(erlang:md5(Secret ++ UserId)).

login_cookies(UserId) ->
  login_cookies(UserId, session_identifier(UserId)).
login_cookies(UserId, SessionId) ->
  [ mochiweb_cookies:cookie("user_id", UserId, [{path, "/"}]),
    mochiweb_cookies:cookie("session_id", SessionId, [{path, "/"}]) ].

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

you_should_write_a_test() ->
  ?assertEqual(
    "No, but I will!",
    "Have you written any tests?"),
  ok.

-endif.
