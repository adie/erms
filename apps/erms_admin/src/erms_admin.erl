%% @author Anton Dieterle <antondie@gmail.com>
%% @copyright 2010 Anton Dieterle <antondie@gmail.com>
%% @doc Administrative server for erms.

-module(erms_admin).
-author("Anton Dieterle <antondie@gmail.com>").

-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(SNAME, {global, ?SERVER}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, users/5, users/6, groups/5, groups/6]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_server:start_link(?SNAME, ?MODULE, [], []).

users(M, R, P, A, U) ->
  continue_if_admin(users, M,R,P,A,U).

users(ok, 'GET', _Request, [], Args, User) ->
  gen_server:call(?SNAME, {list_users});
users(ok, 'GET', _Request, [Id], Args, User) ->
  gen_server:call(?SNAME, {get_user, Id});
users(ok, 'POST', _Request, [], Args, User) ->
  gen_server:call(?SNAME, {create_user, Args});
users(ok, 'POST', _Request, [Id], Args, User) ->
  gen_server:call(?SNAME, {update_user, Id, Args}).

groups(M, R, P, A, U) ->
  continue_if_admin(groups, M,R,P,A,U).

groups(ok, 'GET', _Request, [], Args, User) ->
  gen_server:call(?SNAME, {list_groups});
groups(ok, 'GET', _Request, [Id], Args, User) ->
  gen_server:call(?SNAME, {get_group, Id});
groups(ok, 'POST', _Request, [], Args, User) ->
  gen_server:call(?SNAME, {create_group, Args});
groups(ok, 'POST', _Request, [Id], Args, User) ->
  gen_server:call(?SNAME, {update_group, Id, Args}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
  process_flag(trap_exit, true),
  {ok, Args}.

handle_call({list_users}, _From, State) ->
  Users = users:list_to_json(users:find()),
  {reply, {response, Users}, State};
handle_call({get_user, Id}, _From, State) ->
  Result = case users:find_id(Id) of
    undefined ->
      {error, list_to_binary("Can't find user with id "++integer_to_list(Id))};
    User ->
      {response, users:to_json(User)}
  end,
  {reply, Result, State};
handle_call({create_user, Args}, _From, State) ->
  Login = proplists:get_value("login", Args),
  Password = proplists:get_value("password", Args),
  Fullname = proplists:get_value("fullname", Args),
  Info = proplists:get_value("info", Args),
  PasswordHash = erms_auth:hash_for(Login, Password),
  User = users:new(Login, PasswordHash, Fullname, Info),
  users:save(User),
  {reply, {response, ok}, State};
handle_call({update_user, Id, Args}, _From, State) ->
  Result = case users:find_id(Id) of
    undefined ->
      {error, list_to_binary("Can't find user with id "++integer_to_list(Id))};
    User ->
      Login = proplists:get_value("login", Args),
      Password = proplists:get_value("password", Args),
      Fullname = proplists:get_value("fullname", Args),
      Info = proplists:get_value("info", Args),
      PasswordHash = erms_auth:hash_for(Login, Password),
      NewUser = users:set_fields(User, [{login, Login}, {password_hash, PasswordHash}, {fullname, Fullname}, {info, Info}]),
      users:save(NewUser),
      {response, ok}
  end,
  {reply, Result, State};

handle_call({list_groups}, _From, State) ->
  Groups = groups:list_to_json(groups:find()),
  {reply, {response, Groups}, State};
handle_call({get_group, Id}, _From, State) ->
  Result = case groups:find_id(Id) of
    undefined ->
      {error, list_to_binary("Can't find group with id "++integer_to_list(Id))};
    Group ->
      {response, groups:to_json(Group)}
  end,
  {reply, Result, State};
handle_call({create_group, Args}, _From, State) ->
  Name = proplists:get_value("name", Args),
  Group = groups:new(Name),
  groups:save(Group),
  {reply, {response, ok}, State};
handle_call({update_group, Id, Args}, _From, State) ->
  Result = case groups:find_id(Id) of
    undefined ->
      {error, list_to_binary("Can't find group with id "++integer_to_list(Id))};
    Group ->
      Name = proplists:get_value("name", Args),
      NewGroup = groups:name(Group, Name),
      groups:save(NewGroup),
      {response, ok}
  end,
  {reply, Result, State};

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

continue_if_admin(Func,M,R,P,A,U) ->
  Groups = users_groups:find({user_id, '=', users:id(U)}),
  GroupIds = lists:map(fun(G) -> users_groups:group_id(G) end, Groups),
  case lists:member(1, GroupIds) of
    true ->
      ?MODULE:Func(ok, M, R, P, A, U);
    false ->
      {error, <<"Access denied">>}
  end.

