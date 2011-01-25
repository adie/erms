-module(api_crypto).

-export([handle/5]).

handle('GET', _Request, ["encrypt"], Args, User) ->
  Data = proplists:get_value("data", Args),
  Key = erms_db:q(users, public_key, User),
  Cipher = erms_crypto:encrypt_public(Data, Key),
  B64 = base64:encode(Cipher),
  {response, B64};

handle('GET', _Request, ["decrypt"], Args, User) ->
  Data = proplists:get_value("data", Args, ""),
  Cipher = base64:decode(list_to_binary(Data)),
  Key = erms_db:q(users, private_key, User),
  Source = erms_crypto:decrypt_private(Cipher, Key),
  {response, Source}.

