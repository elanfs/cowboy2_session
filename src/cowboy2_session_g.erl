% common functions
-module(cowboy2_session_g).

-export([
    unixtime/0
  , get_session_secret/0
  , gen_session_id/0
  , encrypt/2
  , decrypt/2
]).

unixtime() ->
  {Mega, Sec, Micro} = os:timestamp(),
  erlang:trunc((Mega * 1000000 + Sec)*1000 + Micro*0.001).

get_session_secret() ->
  case mnesia:dirty_read(cowboy2_session_secret, 0) of
    [{_,_, Secret}] -> Secret;
    _ -> throw(not_found)
  end.

gen_session_id() ->
  NumBytes = 4,
  list_to_binary(integer_to_list(
    crypto:bytes_to_integer(crypto:strong_rand_bytes(NumBytes)))).


encrypt(Key, Plain) ->
  Cipher = crypto:crypto_one_time(rc4, Key, Plain, [{encrypt, true}]),
  base64:encode(Cipher).

decrypt(Key, Base64Cipher) ->
  crypto:crypto_one_time(rc4, Key, base64:decode(Base64Cipher), [{encrypt, false}]).
