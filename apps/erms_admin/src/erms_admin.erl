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

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_server:start_link(?SNAME, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
  process_flag(trap_exit, true),
  {ok, Args}.

%%% Users
handle_call({list_users}, _From, State) ->
  Users = q(users, list_to_json, q(users, find)),
  {reply, {response, Users}, State};
handle_call({get_user, Id}, _From, State) ->
  Result = case q(users, find_id, Id) of
    undefined ->
      {error, list_to_binary(lists:concat(["Can't find user with id ", Id]))};
    User ->
      {response, q(users, to_json_with_groups, User)}
  end,
  {reply, Result, State};
handle_call({create_user, Args}, _From, State) ->
  Login = proplists:get_value("login", Args),
  Password = proplists:get_value("password", Args),
  Fullname = proplists:get_value("fullname", Args),
  Info = proplists:get_value("info", Args),
  PasswordHash = erms_auth:hash_for(Login, Password),
  User = q(users, new_with, [
      {login,Login},
      {password_hash,PasswordHash},
      {fullname, Fullname},
      {info, Info}
    ]),
  q(users, save, User),
  {reply, {response, ok}, State};
handle_call({update_user, Id, Args}, _From, State) ->
  Result = case q(users, find_id, Id) of
    undefined ->
      {error, list_to_binary(lists:concat(["Can't find user with id ", Id]))};
    User ->
      Login = proplists:get_value("login", Args, q(users, login, User)),
      PasswordHash = case proplists:get_value("password", Args) of
        undefined -> q(users, password_hash, User);
        Password -> erms_auth:hash_for(Login, Password)
      end,
      Fullname = proplists:get_value("fullname", Args, q(users, fullname, User)),
      Info = proplists:get_value("info", Args, q(users, info, User)),
      PrivKey = proplists:get_value("private_key", Args, q(users, private_key, User)),
      PubKey = proplists:get_value("public_key", Args, q(users, public_key, User)),
      NewUser = q(users, set_fields, User, [{login, Login}, {password_hash, PasswordHash}, {fullname, Fullname}, {info, Info}, {private_key, PrivKey}, {public_key, PubKey}]),
      q(users, save, NewUser),
      {response, ok}
  end,
  {reply, Result, State};

%%% Groups
handle_call({list_groups}, _From, State) ->
  Groups = q(groups, list_to_json, q(groups, find)),
  {reply, {response, Groups}, State};
handle_call({get_group, Id}, _From, State) ->
  Result = case q(groups, find_id, Id) of
    undefined ->
      {error, list_to_binary(lists:concat(["Can't find group with id ", Id]))};
    Group ->
      {response, q(groups, to_json_with_users, Group)}
  end,
  {reply, Result, State};
handle_call({create_group, Args}, _From, State) ->
  Name = proplists:get_value("name", Args),
  Group = q(groups, new, Name),
  q(groups, save, Group),
  {reply, {response, ok}, State};
handle_call({update_group, Id, Args}, _From, State) ->
  Result = case q(groups, find_id, Id) of
    undefined ->
      {error, list_to_binary(lists:concat(["Can't find group with id ", Id]))};
    Group ->
      Name = proplists:get_value("name", Args, q(groups, name, Group)),
      NewGroup = q(groups, name, Group, Name),
      q(groups, save, NewGroup),
      {response, ok}
  end,
  {reply, Result, State};

%% Users in groups
handle_call({list_users_groups}, _From, State) ->
  Groups = q(users_groups, list_to_json, q(users_groups, find)),
  {reply, {response, Groups}, State};
handle_call({create_users_groups, Args}, _From, State) ->
  UserId = proplists:get_value("user_id", Args),
  GroupId = proplists:get_value("group_id", Args),
  UG = q(users_groups, new, UserId, GroupId),
  q(users_groups, save, UG),
  {reply, {response, ok}, State};
handle_call({delete_users_groups, Id}, _From, State) ->
  Result = case q(users_groups, find_id, Id) of
    undefined ->
      {error, list_to_binary(lists:concat(["Can't find user group with id ", Id]))};
    UG ->
      q(users_groups, delete, UG),
      {response, ok}
  end,
  {reply, Result, State};

%%% Groups for folders
handle_call({list_folder_groups}, _From, State) ->
  Groups = q(doc_folder_groups, list_to_json, q(doc_folder_groups, find)),
  {reply, {response, Groups}, State};
handle_call({create_folder_groups, Args}, _From, State) ->
  FolderId = proplists:get_value("doc_folder_id", Args),
  GroupId = proplists:get_value("group_id", Args),
  FG = q(doc_folder_groups, new, FolderId, GroupId),
  q(doc_folder_groups, save, FG),
  {reply, {response, ok}, State};
handle_call({delete_folder_groups, Id}, _From, State) ->
  Result = case q(doc_folder_groups, find_id, Id) of
    undefined ->
      {error, list_to_binary(lists:concat(["Can't find folder group with id ", Id]))};
    FG ->
      q(doc_folder_groups, delete, FG),
      {response, ok}
  end,
  {reply, Result, State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

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

q(Model, Method) ->
  erms_db:q(Model, Method).
q(Model, Method, Params) ->
  erms_db:q(Model, Method, Params).
q(Model, Method, Param1, Param2) ->
  erms_db:q(Model, Method, Param1, Param2).

