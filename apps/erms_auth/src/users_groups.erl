-module(users_groups).
-author("Anton Dieterle <antondie@gmail.com>").
-export([fields/0]).

fields() ->
  [id, user_id, group_id].

