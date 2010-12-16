-module(users_groups).
-author("Anton Dieterle <antondie@gmail.com>").
-export([fields/0, list_to_json/1, to_json/1]).

fields() ->
  [id, user_id, group_id].

list_to_json(UsersGroups) ->
  lists:map(fun(UG) -> users_groups:to_json(UG) end, UsersGroups).

to_json(UserGroup) ->
  [{user_group, [
      {id, users_groups:id(UserGroup)},
      lists:nth(1,users:to_json(users:find_id(users_groups:user_id(UserGroup)))),
      lists:nth(1,groups:to_json(groups:find_id(users_groups:group_id(UserGroup))))
  ]}].
