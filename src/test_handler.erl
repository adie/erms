-module(test_handler).
-behaviour(gen_server).

-export([start_link/0]).
-export([process/5]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

start_link() ->
  gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

process(Iteration, Data, Signature, User, Destination) ->
  gen_server:cast({global, ?MODULE}, {process, Iteration, Data, Signature, User, Destination}).

%% gen_server callbacks

init(Args) ->
  process_flag(trap_exit, true),
  {ok, Args}.

handle_call(_Msg, _From, State) ->
  {noreply, State}.

handle_cast({process, Iteration, Data, Signature, User, Destination}, State) ->
  spawn_link(fun() ->
    erms_log:log({test, Destination, Iteration, started}),
    PrivKey = erms_db:q(users, private_key, User),
    PubKey = erms_db:q(users, public_key, User),
    FurtherMsg = [{verify, Signature, PubKey}, {sign, PrivKey}, {encrypt_public, PubKey}],
    %Binary =
    erms_crypto:decrypt_private(Data, PrivKey, Iteration, FurtherMsg),
    %erms_log:log({test, Destination, Iteration, decrypted}),
    %Verified = erms_digsig:verify(Binary, Signature, PubKey, Iteration),
    %erms_log:log({test, Destination, Iteration, verified}),

    %NewSignature = erms_digsig:sign(Binary, PrivKey, Iteration),
    %erms_log:log({test, Destination, Iteration, signed}),
    %NewCipher = erms_crypto:encrypt_public(Binary, PubKey, Iteration),
    %erms_log:log({test, Destination, Iteration, encrypted}),
    ok
  end),
  {noreply, State};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Msg, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

%% Internal functions

