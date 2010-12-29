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

-export([start_link/0, users/5, groups/5, users_groups/5, folder_groups/5]).

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
  continue_if_admin(fun users/6, M,R,P,A,U).

users(ok, 'GET', _Request, [], _Args, _User) ->
  gen_server:call(?SNAME, {list_users});
users(ok, 'GET', _Request, [Id], _Args, _User) ->
  gen_server:call(?SNAME, {get_user, Id});
users(ok, 'POST', _Request, [], Args, _User) ->
  gen_server:call(?SNAME, {create_user, Args});
users(ok, 'POST', _Request, [Id], Args, _User) ->
  gen_server:call(?SNAME, {update_user, Id, Args});
users(_,_,_,_,_,_) -> ok.

groups(M, R, P, A, U) ->
  continue_if_admin(fun groups/6, M,R,P,A,U).

groups(ok, 'GET', _Request, [], _Args, _User) ->
  gen_server:call(?SNAME, {list_groups});
groups(ok, 'GET', _Request, [Id], _Args, _User) ->
  gen_server:call(?SNAME, {get_group, Id});
groups(ok, 'POST', _Request, [], Args, _User) ->
  gen_server:call(?SNAME, {create_group, Args});
groups(ok, 'POST', _Request, [Id], Args, _User) ->
  gen_server:call(?SNAME, {update_group, Id, Args});
groups(_,_,_,_,_,_) -> ok.

users_groups(M, R, P, A, U) ->
  continue_if_admin(fun users_groups/6, M,R,P,A,U).

users_groups(ok, 'GET', _Request, [], _Args, _User) ->
  gen_server:call(?SNAME, {list_users_groups});
users_groups(ok, 'POST', _Request, [], Args, _User) ->
  gen_server:call(?SNAME, {create_users_groups, Args});
users_groups(ok, 'DELETE', _Request, [Id], _Args, _User) ->
  gen_server:call(?SNAME, {delete_users_groups, Id});
users_groups(_,_,_,_,_,_) -> ok.

folder_groups(M, R, P, A, U) ->
  continue_if_admin(fun folder_groups/6, M,R,P,A,U).

folder_groups(ok, 'GET', _Request, [], _Args, _User) ->
  gen_server:call(?SNAME, {list_folder_groups});
folder_groups(ok, 'POST', _Request, [], Args, _User) ->
  gen_server:call(?SNAME, {create_folder_groups, Args});
folder_groups(ok, 'DELETE', _Request, [Id], _Args, _User) ->
  gen_server:call(?SNAME, {delete_folder_groups, Id});
folder_groups(_,_,_,_,_,_) -> ok.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
  process_flag(trap_exit, true),
  {ok, Args}.

%%% Users
handle_call({list_users}, _From, State) ->
  Users = users:list_to_json(users:find()),
  {reply, {response, Users}, State};
handle_call({get_user, Id}, _From, State) ->
  Result = case users:find_id(Id) of
    undefined ->
      {error, list_to_binary(lists:concat(["Can't find user with id ", Id]))};
    User ->
      {response, users:to_json_with_groups(User)}
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
      {error, list_to_binary(lists:concat(["Can't find user with id ", Id]))};
    User ->
      Login = proplists:get_value("login", Args, users:login(User)),
      PasswordHash = case proplists:get_value("password", Args) of
        undefined -> users:password_hash(User);
        Password -> erms_auth:hash_for(Login, Password)
      end,
      Fullname = proplists:get_value("fullname", Args, users:fullname(User)),
      Info = proplists:get_value("info", Args, users:info(User)),
      NewUser = users:set_fields(User, [{login, Login}, {password_hash, PasswordHash}, {fullname, Fullname}, {info, Info}]),
      users:save(NewUser),
      {response, ok}
  end,
  {reply, Result, State};

%%% Groups
handle_call({list_groups}, _From, State) ->
  Groups = groups:list_to_json(groups:find()),
  {reply, {response, Groups}, State};
handle_call({get_group, Id}, _From, State) ->
  Result = case groups:find_id(Id) of
    undefined ->
      {error, list_to_binary(lists:concat(["Can't find group with id ", Id]))};
    Group ->
      {response, groups:to_json_with_users(Group)}
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
      {error, list_to_binary(lists:concat(["Can't find group with id ", Id]))};
    Group ->
      Name = proplists:get_value("name", Args, groups:name(Group)),
      NewGroup = groups:name(Group, Name),
      groups:save(NewGroup),
      {response, ok}
  end,
  {reply, Result, State};

%% Users in groups
handle_call({list_users_groups}, _From, State) ->
  Groups = users_groups:list_to_json(users_groups:find()),
  {reply, {response, Groups}, State};
handle_call({create_users_groups, Args}, _From, State) ->
  UserId = proplists:get_value("user_id", Args),
  GroupId = proplists:get_value("group_id", Args),
  UG = users_groups:new(UserId, GroupId),
  users_groups:save(UG),
  {reply, {response, ok}, State};
handle_call({delete_users_groups, Id}, _From, State) ->
  Result = case users_groups:find_id(Id) of
    undefined ->
      {error, list_to_binary(lists:concat(["Can't find user group with id ", Id]))};
    UG ->
      users_groups:delete(UG),
      {response, ok}
  end,
  {reply, Result, State};

%%% Groups for folders
handle_call({list_folder_groups}, _From, State) ->
  Groups = doc_folder_groups:list_to_json(doc_folder_groups:find()),
  {reply, {response, Groups}, State};
handle_call({create_folder_groups, Args}, _From, State) ->
  FolderId = proplists:get_value("doc_folder_id", Args),
  GroupId = proplists:get_value("group_id", Args),
  FG = doc_folder_groups:new(FolderId, GroupId),
  doc_folder_groups:save(FG),
  {reply, {response, ok}, State};
handle_call({delete_folder_groups, Id}, _From, State) ->
  Result = case doc_folder_groups:find_id(Id) of
    undefined ->
      {error, list_to_binary(lists:concat(["Can't find folder group with id ", Id]))};
    FG ->
      doc_folder_groups:delete(FG),
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
      Func(ok, M, R, P, A, U);
    false ->
      {error, <<"Access denied">>}
  end.

