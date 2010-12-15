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
  Request = simple_bridge:make_request(mochiweb_request_bridge, {Req, DocRoot}),
  Response = simple_bridge:make_response(mochiweb_response_bridge, {Req, DocRoot}),
  try
    case string:tokens(Request:path(), "/") of
      ["test" | Params] ->
        Path = "/"++string:join(Params, "/"),
        case Req:get(method) of
          Method when Method =:= 'GET'; Method =:= 'HEAD' ->
            case Path of
              "/logout" ->
                Resp = Response:cookie("session_id", ""),
                Resp1 = Resp:status_code(302),
                Resp2 = Resp1:header("Location", "/test"),
                Resp3 = Resp2:data("ok"),
                Resp3:build_response();
              "/" ->
                SessionId = proplists:get_value("session_id", Request:cookies()),
                Vars = case SessionId of
                    undefined ->
                      [];
                    SID ->
                      inets:start(),
                      {ok, {_,_Headers,Json}} = httpc:request("http://localhost:8080/erms_dms/folder?session_id="++SID),
                      {struct, List} = mochijson2:decode(Json),
                      [{session_id, SessionId},{list, List}]
                  end,
                Req:ok({"text/html", [], render(Path, DocRoot, Vars)});
              "/"++FilePath ->
                Req:serve_file(FilePath, DocRoot)
            end;
          'POST' ->
            case Path of
              "/" ->
                Post = Request:post_params(),
                Login = proplists:get_value("login", Post),
                Password = proplists:get_value("password", Post),
                inets:start(),
                {ok, {_,_Headers,Json}} = httpc:request("http://localhost:8080/login/"++Login++"/"++Password),
                {struct, List} = mochijson2:decode(Json),
                case proplists:get_value(<<"session_id">>, List) of
                  undefined ->
                    Resp = Response:data("error"),
                    Resp1 = Resp:status_code(302),
                    Resp2 = Resp1:header("Location", "/test/login.html"),
                    Resp2:build_response();
                  SessionId ->
                    Resp = Response:data("ok"),
                    Resp1 = Resp:status_code(302),
                    Resp2 = Resp1:header("Location", "/test"),
                    Resp3 = Resp2:cookie("session_id", SessionId),
                    Resp3:build_response()
                end;
              _ ->
                Req:not_found()
            end;
          _ ->
            Req:respond({501, [], []})
        end;
      Params ->
        erms_api:process_api(Req, DocRoot, Request, Params)
    end
  catch
    Type:What ->
      Report = ["web request failed",
        {path, Request:path()},
        {type, Type}, {what, What},
        {trace, erlang:get_stacktrace()}],
      error_logger:error_report(Report),
      Req:respond({500, [{"Content-Type", "text/plain"}],
          "request failed, sorry\n"})
end.

%% Internal API

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
