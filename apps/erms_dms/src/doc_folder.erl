-module(doc_folder).
-author("Anton Dieterle <antondie@gmail.com>").
-export([fields/0, to_json/1]).

fields() ->
  [id, name, parent_folder_id, created_at].

to_json(Folder) ->
  [{folder, [
    {id, doc_folder:id(Folder)},
    {name, doc_folder:name(Folder)}
  ]}].

