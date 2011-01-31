-module(erms_df).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {socket,received = []}).

start_link() ->
  gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

%% gen_server callbacks

init(_Args) ->
  process_flag(trap_exit, true),
  mbcs:start(),
  {ok, Sock} = gen_tcp:connect("192.168.1.11", 22003, [binary, {packet, 0}, {active, false}]),
  send(Sock, "ROOT"),
  send(Sock, "2009.11.25"),
  recv(Sock),
  send(Sock, "tralicks"),
  recv(Sock),
  {ok, #state{socket=Sock}}.

handle_call(do, _From, #state{socket=Sock} = State) ->
  send(Sock, "<?xml version=\"1.0\" charset=\"Windows-1251\"?><request command=\"EXECUTE_QUERY\" name=\"EXPLORER_TREE_GET\" par2=\"\" par3=\"1\" par4=\"\" ></request>"),
  {ok, Packet} = recv(Sock),
  Resp = mbcs:encode(mbcs:decode(Packet, cp1251), utf8),
  {reply, {xml, Resp}, State};
handle_call(get, _From, #state{socket=Sock,received=Recv}) ->
  {reply, {response, Recv}, #state{socket=Sock}}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({tcp, _Socket, Data}, #state{received=Recv} = State) ->
  {noreply, State#state{received=lists:append(Recv, [Data])}}.

terminate(_Reason, #state{socket=Sock}) ->
  gen_tcp:close(Sock),
  ok.

%% Internal functions

send(Sock, Msg) ->
  Header = io_lib:format("1~10w", [length(Msg)]),
  gen_tcp:send(Sock, Header),
  gen_tcp:send(Sock, Msg).

recv(Sock) ->
  {ok, Header} = gen_tcp:recv(Sock, 11),
  Len = list_to_integer(string:strip(string:substr(binary_to_list(Header), 2, 10))),
  gen_tcp:recv(Sock, Len).

