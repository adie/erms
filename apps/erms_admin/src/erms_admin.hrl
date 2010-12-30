continue_if_admin(M,R,P,A,U) ->
  Groups = users_groups:find({user_id, '=', users:id(U)}),
  GroupIds = lists:map(fun(G) -> users_groups:group_id(G) end, Groups),
  case lists:member(1, GroupIds) of
    true ->
      handle(ok, M, R, P, A, U);
    false ->
      {error, <<"Access denied">>}
  end.

