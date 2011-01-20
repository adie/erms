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

start_link() ->
  gen_server:start_link(?SNAME, ?MODULE, [], []).

sign(What, PrivateKey) ->
  gen_server:call(?SNAME, {sign, What, PrivateKey}).

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
  Signature = crypto:rsa_sign(What, PrivateKey),
  {reply, Signature, State};

handle_call({verify, What, Signature, PublicKey}, _From, State) ->
  Verified = crypto:rsa_verify(What, Signature, PublicKey),
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

