%% @author Anton Dieterle <antondie@gmail.com>
%% @copyright 2010 Anton Dieterle <antondie@gmail.com>

%% @doc DMS server for erms.

-module(erms_dms).
-author("Anton Dieterle <antondie@gmail.com>").

-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(SNAME, {global, ?SERVER}).

-record(state, {file_storage}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, folder/5, document/5]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_server:start_link(?SNAME, ?MODULE, [], []).

folder('GET', Request, [], Args, User) ->
  folder('GET', Request, [0], Args, User);
folder('GET', _Request, [Id], _Args, User) ->
  gen_server:call(?SNAME, {get_folder, Id, User}).

document('GET', _Request, [Id], _Args, _User) ->
  gen_server:call(?SNAME, {get_document, Id});

document('POST', Request, [], Args, User) ->
  [File|_] = Request:post_files(),
  gen_server:call(?SNAME, {create_document, Args, File, User}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
  process_flag(trap_exit, true),
  {ok, Storage} = application:get_env(erms_dms, file_storage),
  file:make_dir(Storage),
  erms_db:code_gen([doc_folder, document]),
  {ok, #state{file_storage=Storage}}.

handle_call({get_folder, Id, User}, _From, State) ->
  Folder = case Id of
    0 ->
      root;
    _ ->
      doc_folder:find_id(Id)
  end,
  Result = case Folder of
      undefined ->
        {error, list_to_binary("Can't find folder with id "++integer_to_list(Id))};
      _ ->
        Folders = lists:map(
          fun(F) ->
              {folder, [{id, doc_folder:id(F)}, {name, doc_folder:name(F)}]}
          end,
          doc_folder:find({parent_folder_id, '=', Id})
        ),
        Documents = lists:map(
          fun(Doc) ->
              {document, [{id, document:id(Doc)}, {name, document:name(Doc)}, {filename, document:filename(Doc)}]}
          end,
          document:find({doc_folder_id, '=', Id})
        ),
        {response, [{folder, Folder}, {subfolders, Folders}, {documents, Documents}]}
  end,
  {reply, Result, State};

handle_call({get_document, Id}, _From, State) ->
  Result = case
    document:find_id(Id) of
      undefined ->
        {error, list_to_binary("Can't find document with id "++integer_to_list(Id))};
      Doc ->
        File = binary_to_list(document:file_path(Doc)),
        {ok, Binary} = file:read_file(File),
        {file, document:filename(Doc), Binary}
    end,
  {reply, Result, State};

handle_call({create_document, Args, File, User}, _From, State) ->
  {uploaded_file, Filename, TempFile, Size} = File,
  Name = proplists:get_value("name", Args),
  {MegaSecs, Secs, _} = now(),
  Timestamp = integer_to_list(MegaSecs*1000000 + Secs),
  NewFileName = State#state.file_storage++"/"++Timestamp++"_"++Filename,
  file:rename(TempFile, NewFileName),
  Now = calendar:universal_time(),
  Doc = document:new(Name, Filename, 0, users:id(User), Now, Now, NewFileName, Size),
  document:save(Doc),
  {reply, {response, ok}, State};
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

