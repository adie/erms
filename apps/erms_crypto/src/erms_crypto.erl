%% @author Anton Dieterle <antondie@gmail.com>
%% @copyright 2011 Anton Dieterle <antondie@gmail.com>
%% @doc erms_crypto server.

-module(erms_crypto).
-author("Anton Dieterle <antondie@gmail.com>").

-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(SNAME, {global, ?SERVER}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([encrypt_private/2, encrypt_public/2, encrypt_public/3, decrypt_private/2, decrypt_private/3, decrypt_public/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-include("../../lib/key_converter.erl").

start_link() ->
  gen_server:start_link(?MODULE, [], []).

encrypt_private(What, Key) when is_list(What) ->
  encrypt_private(list_to_binary(What), Key);
encrypt_private(What, Key) ->
  gen_server:call(server_pid(), {encrypt_private, What, Key}, infinity).

encrypt_public(What, Key) ->
  encrypt_public(What, Key, nolog).
encrypt_public(What, Key, Iteration) when is_list(What) ->
  encrypt_public(list_to_binary(What), Key, Iteration);
encrypt_public(What, Key, Iteration) ->
  gen_server:call(server_pid(), {encrypt_public, What, Key, Iteration}, infinity).

decrypt_private(What, Key) ->
  decrypt_private(What, Key, nolog).
decrypt_private(What, Key, Iteration) when is_list(What) ->
  decrypt_private(list_to_binary(What), Key, Iteration);
decrypt_private(What, Key, Iteration) ->
  gen_server:call(server_pid(), {decrypt_private, What, Key, Iteration}, infinity).

decrypt_public(What, Key) when is_list(What) ->
  decrypt_public(list_to_binary(What), Key);
decrypt_public(What, Key) ->
  gen_server:call(server_pid(), {decrypt_public, What, Key}, infinity).

server_pid() ->
  pg2:get_closest_pid(?MODULE).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
  process_flag(trap_exit, true),
  pg2:join(?MODULE, self()),
  {ok, Args}.

handle_call({encrypt_private, What, Key}, _From, State) ->
  PKey = read_rsa_private_key(Key),
  Cipher = public_key:encrypt_private(What, PKey),
  {reply, Cipher, State};
handle_call({encrypt_public, What, Key, Iteration}, _From, State) ->
  case Iteration of
    nolog -> ok;
    _ -> erms_log:log({test, "", Iteration, encrypt_started})
  end,
  PKey = read_rsa_public_key(Key),
  Cipher = stream_encrypt_public(What, PKey),
  case Iteration of
    nolog -> ok;
    _ -> erms_log:log({test, "", Iteration, encrypted})
  end,
  {reply, Cipher, State};

handle_call({decrypt_private, What, Key, Iteration}, _From, State) ->
  case Iteration of
    nolog -> ok;
    _ -> erms_log:log({test, "", Iteration, decrypt_started})
  end,
  PKey = read_rsa_private_key(Key),
  Source = stream_decrypt_private(What, PKey),
  case Iteration of
    nolog -> ok;
    _ -> erms_log:log({test, "", Iteration, decrypted})
  end,
  {reply, Source, State};
handle_call({decrypt_public, What, Key}, _From, State) ->
  PKey = read_rsa_public_key(Key),
  Source = public_key:decrypt_public(What, PKey),
  {reply, Source, State};

handle_call(_Request, _From, State) ->
  {noreply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

stream_encrypt_public(Bin, Key) ->
  stream_encrypt_public(Bin, <<>>, Key).
stream_encrypt_public(<<>>, Acc, _Key) ->
  Acc;

stream_encrypt_public(Bin, Acc, Key) when byte_size(Bin) > 50 ->
  Data = binary_part(Bin, 0, 50),
  Rest = binary_part(Bin, 50, byte_size(Bin)-50),
  stream_encrypt_public(Data, Rest, Acc, Key);
stream_encrypt_public(Bin, Acc, Key) ->
  stream_encrypt_public(Bin, <<>>, Acc, Key).

stream_encrypt_public(Data, Rest, Acc, Key) ->
  Cipher = public_key:encrypt_public(Data, Key),
  stream_encrypt_public(Rest, <<Acc/binary, Cipher/binary>>, Key).

stream_decrypt_private(Cipher, Key) ->
  stream_decrypt_private(Cipher, Key, <<>>).

stream_decrypt_private(<<>>, _, Result) ->
  Result;
stream_decrypt_private(Bin, Key, Result) ->
  Cipher = binary_part(Bin, 0, 64),
  Rest = binary_part(Bin, 64, byte_size(Bin)-64),
  Data = public_key:decrypt_private(Cipher, Key),
  stream_decrypt_private(Rest, Key, <<Result/binary, Data/binary>>).

