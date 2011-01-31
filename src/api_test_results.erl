-module(api_test_results).
-author("Anton Dieterle <antondie@gmail.com>").

-export([handle/5]).

handle('GET', _Request, [], _Args, _User) ->
  Log = actions_log:find_first({uri, '=', "/run_test/"}, {order_by, {id, desc}}),
  Id = actions_log:id(Log),
  Logs = actions_log:find({{id, '>', Id}, 'and', {ip, '=', "test"}}, {order_by, [post_params, created_at]}),
  {response, list_to_json(Logs)}.

list_to_json(Logs) ->
  lists:map(fun(L) -> to_json(L) end, Logs).

to_json(Log) ->
  [{log_entry, [
      {num, actions_log:post_params(Log)},
      {action, actions_log:result(Log)},
      {time, actions_log:message(Log)}
  ]}].

