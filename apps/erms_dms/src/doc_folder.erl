-module(doc_folder).
-author("Anton Dieterle <antondie@gmail.com>").
-export([fields/0]).

fields() ->
  [id, name, parent_folder_id, created_at].

