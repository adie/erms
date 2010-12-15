%% @author Anton Dieterle <antondie@gmail.com>
%% @copyright 2010 Anton Dieterle <antondie@gmail.com>

%% @doc Session store for erms.

-module(erms_session_store).
-author("Anton Dieterle <antondie@gmail.com>").

-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(SNAME, {global, ?SERVER}).

-record(state, {ets=ets:new(sessions, []), secret_string = "", expire_after=3600}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start/0, create/1, read/1, destroy/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start() ->
  gen_server:start_link(?SNAME, ?MODULE, [], []).

create(UserId) ->
  gen_server:call(?SNAME, {create, UserId}).

read(SessionId) ->
  gen_server:call(?SNAME, {read, SessionId}).

destroy(SessionId) ->
  gen_server:call(?SNAME, {destroy, SessionId}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
  process_flag(trap_exit, true),
  {ok, [{secret, Secret}, {expire_after, ExpireAfter}]} = application:get_env(erms, session_store),
  State = #state{secret_string=Secret, expire_after=ExpireAfter},
  erlang:send_after(ExpireAfter*1000, self(), expire_sessions),
  {ok, State}.

handle_call({create, UserId}, _From, #state{ets=Ets, secret_string=Secret}=State) ->
  SessionId = session_identifier(UserId, Secret),
  ets:insert(Ets, {list_to_binary(SessionId), UserId, unix_timestamp(now())}),
  {reply, SessionId, State};

handle_call({read, SessionId}, _From, #state{ets=Ets, expire_after=ExpireAfter}=State) ->
  SessId = list_to_binary(SessionId),
  case ets:lookup(Ets, SessId) of
    [{_SessId, UserId, _Timestamp}] ->
      case expire(Ets, SessId, ExpireAfter) of
        not_old ->
          ets:insert(Ets, {SessId, UserId, unix_timestamp(now())}),
          {reply, UserId, State};
        expired ->
          {reply, expired, State}
      end;
    _ ->
      {reply, not_found, State}
  end;
handle_call({destroy, SessionId}, _From, #state{ets=Ets}=State) ->
  ets:delete(Ets, list_to_binary(SessionId)),
  {reply, ok, State};
handle_call(_Request, _From, State) ->
  {noreply, ok, State}.

handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(expire_sessions, #state{ets=Ets, expire_after=ExpireAfter}=State) ->
  expire_sessions(Ets, ExpireAfter),
  erlang:send_after(ExpireAfter*1000, self(), expire_sessions),
  {noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

session_identifier(UserId, Secret) when is_integer(UserId) ->
  session_identifier(integer_to_list(UserId), Secret);
session_identifier(UserId, Secret) ->
  mochihex:to_hex(erlang:md5(Secret ++ UserId ++ integer_to_list(unix_timestamp(now())))).

unix_timestamp({MegaSecs, Secs, _}) ->
  MegaSecs*1000000 + Secs.

expire_sessions(Ets, ExpireAfter) ->
  expire_sessions(Ets, ets:first(Ets), ExpireAfter).

expire_sessions(_Ets, '$end_of_table', _ExpireAfter) ->
  ok;
expire_sessions(Ets, Key, ExpireAfter) ->
  NextKey = ets:next(Ets, Key),
  expire(Ets, Key, ExpireAfter),
  expire_sessions(Ets, NextKey, ExpireAfter).

expire(Ets, Key, ExpireAfter) ->
  [{_,_,Timestamp}] = ets:lookup(Ets, Key),
  Now = unix_timestamp(now()),
  expire_if(Ets, Key, Now - Timestamp < ExpireAfter).

expire_if(_Ets, _Key, true) ->
  not_old;
expire_if(Ets, Key, false) ->
  ets:delete(Ets, Key),
  expired.

