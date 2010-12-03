-module(erms_core).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(SNAME, {global, ?SERVER}).
-record(state, {msgList=[]}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, send/1, get_all_msg/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_server:start_link(?SNAME, ?MODULE, [], []).

send(Msg) ->
  gen_server:call(?SNAME, {send, {node(), Msg}}).

get_all_msg() ->
  gen_server:call(?SNAME, get_all_msg).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
  {ok, #state{}}.

handle_call({send, {Node, Msg}}, _From, State) ->
  io:format("[~p]: ~p~n", [Node, Msg]),
  NewMsgList = [{Node, Msg}|State#state.msgList],
  {reply, sent, State#state{msgList=NewMsgList}};
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

