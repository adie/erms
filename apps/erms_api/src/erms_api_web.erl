%% @author Anton Dieterle <antondie@gmail.com>
%% @copyright 2011 Anton Dieterle <antondie@gmail.com>

%% @doc Web server for erms.

-module(erms_api_web).
-author("Anton Dieterle <antondie@gmail.com>").

-export([start/1, stop/0, loop/2]).

%% External API

start(Options) ->
  {DocRoot, Options1} = get_option(docroot, Options),
  Loop = fun (Req) ->
    Request = simple_bridge:make_request(mochiweb_request_bridge, {Req, DocRoot}),
    Response = simple_bridge:make_response(mochiweb_response_bridge, {Req, DocRoot}),
    ?MODULE:loop(Request, Response)
  end,
  mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
  mochiweb_http:stop(?MODULE).

loop(Request, Response) ->
  try
    case string:tokens(Request:path(), "/") of
      ["test" | _] ->
        Resp1 = Response:file(Request:path()),
        Resp1:build_response();
      Params ->
        erms_api:process_api(Request, Response, Params)
    end
  catch Type:What ->
      Report = ["web request failed",
        {path, Request:path()},
        {type, Type}, {what, What},
        {trace, erlang:get_stacktrace()}],
      error_logger:error_report(Report),
      ErrResponse = Response:status_code(500),
      ErrResponse1 = ErrResponse:data("Internal Server Error"),
      ErrResponse1:build_response()
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
