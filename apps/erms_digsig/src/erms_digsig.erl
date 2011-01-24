%% @author Anton Dieterle <antondie@gmail.com>
%% @copyright 2011 Anton Dieterle <antondie@gmail.com>
%% @doc erms_digsig server.

-module(erms_digsig).
-author("Anton Dieterle <antondie@gmail.com>").

-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(SNAME, {global, ?SERVER}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([sign/2, verify/3]).

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

sign(What, PrivateKey) when is_list(What) ->
  sign(list_to_binary(What), PrivateKey);
sign(What, PrivateKey) ->
  gen_server:call(?SNAME, {sign, What, PrivateKey}).

verify(What, Signature, PublicKey) when is_list(What) ->
  verify(list_to_binary(What), Signature, PublicKey);
verify(What, Signature, PublicKey) ->
  gen_server:call(?SNAME, {verify, What, Signature, PublicKey}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
  process_flag(trap_exit, true),
  crypto:start(),
  {ok, Args}.

handle_call({sign, What, PrivateKey}, _From, State) ->
  Key = read_rsa_private_key(PrivateKey),
  Signature = public_key:sign(What, sha, Key),
  {reply, Signature, State};

handle_call({verify, What, Signature, PublicKey}, _From, State) ->
  Key = read_rsa_public_key(PublicKey),
  Verified = public_key:verify(What, sha, Signature, Key),
  {reply, Verified, State};

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

