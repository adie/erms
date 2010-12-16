-module(doc_folder_groups).
-author("Anton Dieterle <antondie@gmail.com>").
-export([fields/0, list_to_json/1, to_json/1]).

fields() ->
  [id, doc_folder_id, group_id].

list_to_json(FolderGroups) ->
  lists:map(fun(FG) -> doc_folder_groups:to_json(FG) end, FolderGroups).

to_json(FolderGroup) ->
  [{folder_group, [
      {id, doc_folder_groups:id(FolderGroup)},
      lists:nth(1,doc_folder:to_json(doc_folder:find_id(doc_folder_groups:doc_folder_id(FolderGroup)))),
      lists:nth(1,groups:to_json(groups:find_id(users_groups:group_id(FolderGroup))))
  ]}].
