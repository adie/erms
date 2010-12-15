-module(document).
-author("Anton Dieterle <antondie@gmail.com>").
-export([fields/0]).

fields() ->
  [id, name, filename, doc_folder_id, user_id, created_at, updated_at, file_path, file_size].

