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
  try
    case string:tokens(Request:path(), "/") of
      ["test" | _] ->
        "/"++Path = Request:path(),
        Req:serve_file(Path, DocRoot);
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
