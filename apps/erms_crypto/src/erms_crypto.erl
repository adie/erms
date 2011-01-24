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
-export([encrypt_private/2, encrypt_public/2, decrypt_private/2, decrypt_public/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-include("../../lib/key_converter.erl").

start_link() ->
  gen_server:start_link(?SNAME, ?MODULE, [], []).

encrypt_private(What, Key) when is_list(What) ->
  encrypt_private(list_to_binary(What), Key);
encrypt_private(What, Key) ->
  gen_server:call(?SNAME, {encrypt_private, What, Key}).

encrypt_public(What, Key) when is_list(What) ->
  encrypt_public(list_to_binary(What), Key);
encrypt_public(What, Key) ->
  gen_server:call(?SNAME, {encrypt_public, What, Key}).

decrypt_private(What, Key) when is_list(What) ->
  decrypt_private(list_to_binary(What), Key);
decrypt_private(What, Key) ->
  gen_server:call(?SNAME, {decrypt_private, What, Key}).

decrypt_public(What, Key) when is_list(What) ->
  decrypt_public(list_to_binary(What), Key);
decrypt_public(What, Key) ->
  gen_server:call(?SNAME, {decrypt_public, What, Key}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
  process_flag(trap_exit, true),
  {ok, Args}.

handle_call({encrypt_private, What, Key}, _From, State) ->
  PKey = read_rsa_private_key(Key),
  Cipher = public_key:encrypt_private(What, PKey),
  {reply, Cipher, State};
handle_call({encrypt_public, What, Key}, _From, State) ->
  PKey = read_rsa_public_key(Key),
  Cipher = public_key:encrypt_public(What, PKey),
  {reply, Cipher, State};

handle_call({decrypt_private, What, Key}, _From, State) ->
  PKey = read_rsa_private_key(Key),
  Source = public_key:decrypt_private(What, PKey),
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

