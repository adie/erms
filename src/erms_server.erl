-module(erms_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start/1, stop/1, send/2, get_all_msg/1, get_users/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% Records
%% ------------------------------------------------------------------
-record(state, {msgList=[]}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start(Name) ->
  gen_server_cluster:start(Name, ?MODULE, [], []).

stop(Name) ->
  gen_server:call({global, Name}, stop).

send(Name, Msg) ->
  gen_server:call({global, Name}, {send, {Name, node(), Msg}}).

get_all_msg(Name) ->
  gen_server:call({global, Name}, get_all_msg).

get_users(Name) ->
  {GlobalNode, LocalNodeList} = gen_server_cluster:get_all_server_nodes(Name),
  [GlobalNode|LocalNodeList].

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
  {ok, #state{}}.

handle_call({send, {Name, Node, Msg}}, _From, _State) ->
  F = fun(State) ->
      io:format("[~p,~p]: ~p~n", [Name, Node, Msg]),
      NewMsgList = [{Node, Msg}|State#state.msgList],
      State#state{msgList=NewMsgList}
  end,
  {reply, sent, F};
handle_call(get_all_msg, _From, State) ->
  List = lists:reverse(State#state.msgList),
  {reply, List, State};
handle_call(stop, _From, State) ->
  {stop, normalStop, State};
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

