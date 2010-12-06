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

-export([start/2, code_gen/1]).

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
handle_call(_Request, _From, State) ->
  {noreply, ok, State}.

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

erlydb_mysql_init(Options) ->
  erlydb:start(mysql, Options).

erlydb_mnesia_init(Options) ->
  erlydb:start(mnesia, Options).

erlydb_psql_init(_Options) ->
  erlydb_psql:start().

connect(#db_info{db=Database, options=Options}) ->
  %Driver = list_to_atom("erlydb_" ++ atom_to_list(Database)),
  case Database of
    mysql ->
      erlydb_mysql_init(Options);
    mnesia ->
      erlydb_mnesia_init(Options);
    pgsql ->
      erlydb_psql_init(Options)
  end,
  ok = code_gen_internal([options], Database),
  ok.

code_gen_internal(Models, Database) ->
  code_gen_internal(Models, Database, []).

code_gen_internal(Models, Database, Options) ->
  erlydb:code_gen(Models, Database, Options).

