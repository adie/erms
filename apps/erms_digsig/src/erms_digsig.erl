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
-export([sign/2, sign/4, verify/3, verify/5]).

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

sign(What, PrivateKey) ->
  sign(What, PrivateKey, nolog, []).
sign(What, PrivateKey, Iteration, FurtherMsg) when is_list(What) ->
  sign(list_to_binary(What), PrivateKey, Iteration, FurtherMsg);
sign(What, PrivateKey, Iteration, FurtherMsg) ->
  gen_server:call(?SNAME, {sign, What, PrivateKey, Iteration, FurtherMsg}).

verify(What, Signature, PublicKey) ->
  verify(What, Signature, PublicKey, nolog, []).
verify(What, Signature, PublicKey, Iteration, FurtherMsg) when is_list(What) ->
  verify(list_to_binary(What), Signature, PublicKey, Iteration, FurtherMsg);
verify(What, Signature, PublicKey, Iteration, FurtherMsg) ->
  gen_server:call(?SNAME, {verify, What, Signature, PublicKey, Iteration, FurtherMsg}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
  process_flag(trap_exit, true),
  crypto:start(),
  {ok, Args}.

handle_call({sign, What, PrivateKey, Iteration, FurtherMsg}, _From, State) ->
  case Iteration of
    nolog -> ok;
    _ -> erms_log:log({test, "", Iteration, sign_started})
  end,
  Key = read_rsa_private_key(PrivateKey),
  Signature = public_key:sign(What, sha, Key),
  case Iteration of
    nolog -> ok;
    _ -> erms_log:log({test, "", Iteration, signed})
  end,
  case FurtherMsg of
    [] -> {reply, Signature, State};
    [{encrypt_public, PubKey}] ->
      spawn(fun() ->
          erms_crypto:encrypt_public(What, PubKey, Iteration)
        end),
      {noreply, State}
  end;

handle_call({verify, What, Signature, PublicKey, Iteration, FurtherMsg}, _From, State) ->
  case Iteration of
    nolog -> ok;
    _ -> erms_log:log({test, "", Iteration, verify_started})
  end,
  Key = read_rsa_public_key(PublicKey),
  Verified = public_key:verify(What, sha, Signature, Key),
  case Iteration of
    nolog -> ok;
    _ -> erms_log:log({test, "", Iteration, verified})
  end,
  case FurtherMsg of
    [] -> {reply, Verified, State};
    [{sign, PrivKey} | NextMsg] ->
      spawn(fun() ->
          erms_digsig:sign(What, PrivKey, Iteration, NextMsg)
        end),
      {noreply, State}
  end;

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

