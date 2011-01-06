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

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_server:start_link(?SNAME, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
  process_flag(trap_exit, true),
  {ok, Storage} = application:get_env(erms_dms, file_storage),
  file:make_dir(Storage),
  erms_db:code_gen([doc_folder, document, doc_folder_groups]),
  {ok, #state{file_storage=Storage}}.

handle_call({get_folder, Id, User}, _From, State) ->
  Folder = case Id of
    0 ->
      root;
    _ ->
      q(doc_folder, name, q(doc_folder, find_id, Id))
  end,
  Result = case Folder of
      undefined ->
        {error, list_to_binary(lists:concat(["Can't find folder with id ", Id]))};
      _ ->
        UserGroups = q(users_groups, find, {user_id, '=', q(users, id, User)}),
        GroupIds = lists:map(fun(G) -> q(users_groups, group_id, G) end, UserGroups),
        FolderGroups = q(doc_folder_groups, find, {doc_folder_id, '=', Id}),
        case compare_groups(GroupIds, FolderGroups) of
          false ->
            {error, <<"Permission denied accessing folder">>};
          true  ->
            Folders = lists:map(
              fun(F) ->
                  [{folder, [{id, q(doc_folder, id, F)}, {name, q(doc_folder, name, F)}]}]
              end,
              lists:filter(
                fun(F) ->
                  FGroups = q(doc_folder_groups, find, {doc_folder_id, '=', q(doc_folder, id, F)}),
                  compare_groups(GroupIds, FGroups)
                end,
                q(doc_folder, find, {parent_folder_id, '=', Id})
              )
            ),
            Documents = lists:map(
              fun(Doc) ->
                  [{document, [{id, q(document, id, Doc)}, {name, q(document, name, Doc)}, {filename, q(document, filename, Doc)}]}]
              end,
              q(document, find, {doc_folder_id, '=', Id})
            ),
            {response, [{folder, [{name, Folder}]}, {subfolders, Folders}, {documents, Documents}]}
        end
  end,
  {reply, Result, State};

handle_call({create_folder, Id, Args, User}, _From, State) ->
  Name = proplists:get_value("name", Args),
  Now = calendar:universal_time(),
  Folder = q(doc_folder, new_with, [{name, Name}, {parent_folder_id, Id}, {created_at, Now}]),
  q(doc_folder, save, Folder),
  {reply, {response, ok}, State};

handle_call({get_document, Id}, _From, State) ->
  Result = case
    q(document, find_id, Id) of
      undefined ->
        {error, list_to_binary(lists:concat(["Can't find document with id ", Id]))};
      Doc ->
        File = binary_to_list(q(document, file_path, Doc)),
        {ok, Binary} = file:read_file(File),
        {file, q(document, filename, Doc), Binary}
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
  Doc = q(document, new_with, [
      {name, Name},
      {filename, Filename},
      {doc_folder_id, 0},
      {user_id, q(users, id, User)},
      {created_at, Now},
      {updated_at, Now},
      {file_path, NewFileName},
      {file_size, Size}
    ]),
  q(document, save, Doc),
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

compare_groups(_GroupIds, FolderGroups) when length(FolderGroups) == 0 ->
  true;
compare_groups(GroupIds, FolderGroups) ->
  lists:any(
    fun(FG) ->
        lists:member(q(doc_folder_groups, group_id, FG), [0|GroupIds])
    end, FolderGroups
  ).

q(Model, Method, Params) ->
  erms_db:q(Model, Method, Params).

