%% @author Anton Dieterle <antondie@gmail.com>
%% @copyright 2010 Anton Dieterle <antondie@gmail.com>
%% @doc erms_log server.

-module(erms_log).
-author("Anton Dieterle <antondie@gmail.com>").

-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(SNAME, {global, ?SERVER}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([log/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_server:start_link(?SNAME, ?MODULE, [], []).

log({test, TestId, Num, Action}) ->
  gen_server:cast(?SNAME, {log, test, TestId, Num, Action});

log({login_failure, Request}) ->
  gen_server:cast(?SNAME, {log, login_failure, Request, Request:request_body()});
log({login_success, Request, User}) ->
  gen_server:cast(?SNAME, {log, login_success, Request, User});
log({request, Request, User}) ->
  gen_server:cast(?SNAME, {log, request, Request, Request:request_body(), User});
log({response, Request, User, Resp}) ->
  gen_server:cast(?SNAME, {log, response, Request, Request:request_body(), User, Resp}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
  process_flag(trap_exit, true),
  erms_db:code_gen([actions_log]),
  {ok, Args}.

handle_call(_Request, _From, State) ->
  {noreply, ok, State}.

handle_cast({log, test, TestId, Num, Action}, State) ->
  {M, S, N} = now(),
  Log = actions_log:new(
    calendar:universal_time(),
    "test", 0, "",
    TestId,
    Num,
    Action,
    lists:concat([M,S,N])
  ),
  actions_log:save(Log),
  {noreply, State};
handle_cast({log, login_failure, Request, ReqBody}, State) ->
  Log = actions_log:new(
    calendar:universal_time(),
    get_ip(Request), 0, "",
    Request:uri(),
    ReqBody,
    failure,
    "Log in failure"
  ),
  actions_log:save(Log),
  {noreply, State};
handle_cast({log, login_success, Request, User}, State) ->
  Log = actions_log:new(
    calendar:universal_time(),
    get_ip(Request), users:id(User), users:login(User),
    Request:uri(),
    "",
    success,
    "Successfully logged in"
  ),
  actions_log:save(Log),
  {noreply, State};
handle_cast({log, request, Request, ReqBody, User}, State) ->
  Log = actions_log:new(
    calendar:universal_time(),
    get_ip(Request), users:id(User), users:login(User),
    Request:path(),
    ReqBody,
    request,
    ""
  ),
  actions_log:save(Log),
  {noreply, State};
handle_cast({log, response, Request, ReqBody, User, Resp}, State) ->
  {_,_,_,{response,200,_,_,{data, Data}}} = Resp,
  Log = actions_log:new(
    calendar:universal_time(),
    get_ip(Request), users:id(User), users:login(User),
    Request:path(),
    ReqBody,
    success,
    re:replace(Data, "'", "\\'", [{return, list}])
  ),
  actions_log:save(Log),
  {noreply, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

get_ip(Request) ->
  {Q,W,E,R} = Request:peer_ip(),
  lists:concat([Q,".",W,".",E,".",R]).

