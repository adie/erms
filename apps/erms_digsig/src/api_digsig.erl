-module(api_digsig).

-export([handle/5]).

handle('GET', _Request, ["sign"], Args, User) ->
  Data = proplists:get_value("data", Args, ""),
  Key = erms_db:q(users, private_key, User),
  Signature = erms_digsig:sign(Data, Key),
  SS = base64:encode(Signature),
  {response, SS};

handle('GET', _Request, ["verify"], Args, User) ->
  Data = proplists:get_value("data", Args, ""),
  Signature = proplists:get_value("signature", Args),
  SS = base64:mime_decode(Signature),
  Key = erms_db:q(users, public_key, User),
  Verified = erms_digsig:verify(Data, SS, Key),
  {response, list_to_binary(atom_to_list(Verified))}.

