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
        case Params of
          [Module,Function|_Options] ->
            M = list_to_atom(Module),
            F = list_to_atom(Function),
            Result = M:F(),
            Req:ok({"text/plain", [],
                mochijson2:encode({struct, [{result, Result}]})
              });
          [Module] ->
            M = list_to_atom(Module),
            case (catch M:api_functions()) of
              {'EXIT', {undef, _}} ->
                Req:ok({"text/plain", [],
                    mochijson2:encode({struct, [{error, <<"No such module">>}]})
                  });
              Funs ->
                Req:ok({"text/plain", [],
                    mochijson2:encode({struct, [{functions, Funs}]})
                  })
              end;
          _ ->
            Req:ok({"text/plain", [], "We're in API!"})
        end;
      _ ->
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
