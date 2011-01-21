-module(users).
-author("Anton Dieterle <antondie@gmail.com>").
-export([fields/0, list_to_json/1, to_json/1, to_json_with_groups/1]).

fields() ->
  [id, login, password_hash, fullname, info, private_key, public_key].

list_to_json(Users) ->
  lists:map(fun(U) -> users:to_json(U) end, Users).

to_json(User) ->
  [{user, [
      {id, users:id(User)},
      {login, users:login(User)},
      {fullname, users:fullname(User)},
      {info, users:info(User)}
  ]}].

to_json_with_groups(User) ->
  UserGroups = groups:list_to_json(users_groups:find({user_id, '=', users:id(User)})),
  [{user, [
      {id, users:id(User)},
      {login, users:login(User)},
      {fullname, users:fullname(User)},
      {info, users:info(User)},
      {groups, UserGroups}
  ]}].

