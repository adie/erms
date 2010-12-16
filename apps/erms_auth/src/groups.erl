-module(groups).
-author("Anton Dieterle <antondie@gmail.com>").
-export([fields/0, list_to_json/1, to_json/1, to_json_with_users/1]).

fields() ->
  [id, name].

list_to_json(Groups) ->
  lists:map(fun(G) -> groups:to_json(G) end, Groups).

to_json(Group) ->
  {group, [
      {id, groups:id(Group)},
      {name, groups:name(Group)}
  ]}.

to_json_with_users(Group) ->
  Users = users:list_to_json(users_groups:find({group_id, '=', groups:id(Group)})),
  {group, [
      {id, groups:id(Group)},
      {name, groups:name(Group)},
      {users, Users}
  ]}.

