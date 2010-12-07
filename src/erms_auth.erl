%% @author Anton Dieterle <antondie@gmail.com>
%% @copyright 2010 Anton Dieterle <antondie@gmail.com>
%% @doc Authentication server for erms.

-module(erms_auth).
-author("Anton Dieterle <antondie@gmail.com>").

-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(SNAME, {global, ?SERVER}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, api_functions/0, test_api/1, check_password/2, hash_for/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_server:start_link(?SNAME, ?MODULE, [], []).

api_functions() ->
  [test_api].

test_api(_Args) ->
  <<"Hello, World!">>.

check_password(Login, Password) ->
  gen_server:call(?SNAME, {check_password, Login, Password}).

hash_for(Name, Password) ->
  Salt = mochihex:to_hex(erlang:md5(Name)),
  list_to_binary(mochihex:to_hex(erlang:md5(Salt ++ Password))).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
  erms_db:code_gen([users]),
  {ok, Args}.

handle_call({check_password, Login, Password}, _From, State) ->
  {reply, internal_check_password(Login, Password), State};
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

internal_check_password(Login, Password) ->
  case users:find_first({login, '=', Login}) of
    undefined ->
      false;
    User ->
      hash_for(Login, Password) =:= users:password_hash(User)
  end.

