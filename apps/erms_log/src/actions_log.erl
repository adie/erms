-module(actions_log).
-author("Anton Dieterle <antondie@gmail.com>").
-export([fields/0, list_to_json/1, to_json/1]).

fields() ->
  [id, created_at, ip, user_id, user_login, uri, post_params, result, message].

list_to_json(Logs) ->
  lists:map(fun(Log) -> actions_log:to_json(Log) end, Logs).

to_json(Log) ->
  [{logged_action, [
      {id, actions_log:id(Log)},
      {created_at, actions_log:created_at(Log)},
      {user_id, actions_log:user_id(Log)},
      {user_login, actions_log:user_login(Log)},
      {request, actions_log:request(Log)},
      {result, actions_log:result(Log)}
  ]}].

