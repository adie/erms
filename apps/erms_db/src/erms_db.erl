%% @author Anton Dieterle <antondie@gmail.com>
%% @copyright 2010 Anton Dieterle <antondie@gmail.com>

%% @doc DB server for erms.

-module(erms_db).
-author("Anton Dieterle <antondie@gmail.com>").

-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(SNAME, {global, ?SERVER}).

-record(db_info, {db, options}).
-record(state, {db_info = #db_info{}}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start/2]).
-export([code_gen/1, q/2, q/3, q/4]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start(Database, Options) ->
  gen_server:start_link(?SNAME, ?MODULE, #db_info{db=Database, options=Options}, []).

code_gen(Models) ->
  gen_server:call(?SNAME, {code_gen, Models}).

q(Model, Method) ->
  gen_server:call(?SNAME, {make_query, Model, Method}).
q(Model, Method, Params) ->
  gen_server:call(?SNAME, {make_query, Model, Method, Params}).
q(Model, Method, Param1, Param2) ->
  gen_server:call(?SNAME, {make_query, Model, Method, Param1, Param2}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(DbInfo) ->
  process_flag(trap_exit, true),
  connect(DbInfo),
  {ok, #state{db_info=DbInfo}}.

handle_call({code_gen, Models}, _From, #state{db_info=DbInfo} = State) ->
  #db_info{db=Database} = DbInfo,
  Result = code_gen_internal(Models, Database),
  {reply, Result, State};

handle_call({make_query, Model, Method}, From, State) ->
  spawn_link(fun() ->
        Result = Model:Method(),
        gen_server:reply(From, Result)
    end),
  {noreply, State};
handle_call({make_query, Model, Method, Params}, From, State) ->
  spawn_link(fun() ->
        Result = Model:Method(Params),
        gen_server:reply(From, Result)
    end),
  {noreply, State};
handle_call({make_query, Model, Method, Param1, Param2}, From, State) ->
  spawn_link(fun() ->
        Result = Model:Method(Param1, Param2),
        gen_server:reply(From, Result)
    end),
  {noreply, State};

handle_call(_Request, _From, State) ->
  {noreply, State}.

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

connect(#db_info{db=Database, options=Options}) ->
  ok = erlydb_init(Database, Options),
  ok = code_gen_internal([options], Database),
  ok.

erlydb_init(mysql, Options) ->
  erlydb:start(mysql, Options);

erlydb_init(mnesia, Options) ->
  erlydb:start(mnesia, Options);

erlydb_init(pgsql, _Options) ->
  erlydb_psql:start().


code_gen_internal(Models, Database) ->
  code_gen_internal(Models, Database, []).

code_gen_internal(Models, Database, Options) ->
  erlydb:code_gen(Models, Database, Options).

