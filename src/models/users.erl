-module(users).
-author("Anton Dieterle <antondie@gmail.com>").
-export([table/0, fields/0]).

table() ->
  user.
fields() ->
  [id, login, password, fullname, info].

