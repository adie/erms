-module(api_run_test).
-author("Anton Dieterle <antondie@gmail.com>").

-export([handle/5]).

handle('GET', _Request, [], Args, User) ->
  Iterations = proplists:get_value("iterations", Args, 20),
  Dest = proplists:get_value("dest", Args, ""),

  Doc = erms_db:q(document, find_first),
  Filename = binary_to_list(erms_db:q(document, file_path, Doc)),
  {ok, Binary} = file:read_file(Filename),
  PubKey = erms_db:q(users, public_key, User),
  PrivKey = erms_db:q(users, private_key, User),
  Cipher = erms_crypto:encrypt_public(Binary, PubKey),
  Signature = erms_digsig:sign(Binary, PrivKey),

  run(Iterations, Cipher, Signature, User, Dest),

  {response, ok}.

run(0, _Cipher, _Signature, _User, _Dest) ->
  ok;
run(Iterations, Cipher, Signature, User, Dest) when is_list(Iterations) ->
  run(list_to_integer(Iterations), Cipher, Signature, User, Dest);
run(Iteration, Cipher, Signature, User, Dest) ->
  test_handler:process(Iteration, Cipher, Signature, User, Dest),
  run(Iteration-1, Cipher, Signature, User, Dest).

