%% @author Anton Dieterle <antondie@gmail.com>
%% @copyright 2011 Anton Dieterle <antondie@gmail.com>
%% @doc Convertion from openssl keys to public_key records

-author("Anton Dieterle <antondie@gmail.com>").

read_rsa_private_key(Key) ->
  [PrivK] = public_key:pem_decode(Key),
  public_key:pem_entry_decode(PrivK).

% Workaround for decoding public key (as for R14B01, future versions
% of Erlang would have better ways to do so). Thanks to Erlang mailing list!
read_rsa_public_key(Key) ->
  Bin = erlang:iolist_to_binary(public_key_lines(re:split(Key, "\n"), [])),
  Spki = public_key:der_decode('SubjectPublicKeyInfo',
  base64:mime_decode(Bin)),
  {_, _, {0, KeyDer}} = Spki,
  public_key:der_decode('RSAPublicKey', KeyDer).

public_key_lines([<<"-----BEGIN PUBLIC KEY-----">>|Rest], Acc) ->
  public_key_lines(Rest, Acc);
public_key_lines([<<"-----END PUBLIC KEY-----">>|_], Acc) ->
  lists:reverse(Acc);
public_key_lines([Line|Rest], Acc) ->
  public_key_lines(Rest, [Line|Acc]).


