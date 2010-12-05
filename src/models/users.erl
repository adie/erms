-module(users).
-author("Anton Dieterle <antondie@gmail.com>").
-export([fields/0]).

fields() ->
  [id, login, password, fullname, info].

