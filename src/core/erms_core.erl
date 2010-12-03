-module(erms_core).
-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(SRV, {global, ?MODULE}).

-include_lib("stdlib/include/qlc.hrl").

-record(rec, {key, value}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start/0, stop/0, get/1, set/2, create_schema/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start() ->
  gen_server:start_link(?SRV, ?MODULE, [], []).

stop() ->
  gen_server:cast(?SRV, stop).

get(Key) ->
  gen_server:call(?SRV, {get, Key}).

set(Key, Value) ->
  gen_server:call(?SRV, {set, Key, Value}).

create_schema() ->
  mnesia:create_schema([node()|nodes()]),
  mnesia:start(),
  lists:foreach(fun(N) ->
        io:format("starting mnesia on ~w~n", [N]),
        rpc:call(N, mnesia, start, [])
    end, nodes()),
  mnesia:create_table(rec, [
      {disc_copies, [node()|nodes()]},
      {attributes, record_info(fields, rec)}
  ]).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
  process_flag(trap_exit, true),
  io:format("erms core starting~n"),
  mnesia:start(),
  ok = mnesia:wait_for_tables([rec], 2000),
  {ok, Args}.

handle_call({get, Key}, _From, State) ->
  Rec = do_get(Key),
  {reply, Rec, State};
handle_call({set, Key, Value}, _From, State) ->
  Rec = do_set(Key, Value),
  {reply, Rec, State};
handle_call(_Request, _From, State) ->
  {noreply, ok, State}.

handle_cast(stop, State) ->
  io:format("erms core stopping~n"),
  {stop, normal, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  io:format("erms core terminating~n"),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

do_get(Key) ->
  Res = mnesia:dirty_read({rec, Key}),
  case Res of
    [] -> undefined;
    [Rec] -> Rec#rec.value
  end.

do_set(Key, Value) ->
  F = fun() ->
      Row = #rec{key=Key, value=Value},
      mnesia:write(Row)
  end,
  {atomic, ok} = mnesia:transaction(F),
  ok.
