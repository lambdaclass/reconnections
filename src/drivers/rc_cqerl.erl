-module(rc_cqerl).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2]).

start_link(#{name := Name} = Args) ->
  gen_server:start_link({local, Name}, ?MODULE, Args, []);

start_link(Args) ->
  % The process is called rc_cqperl because the driver already start a cqerl process
  gen_server:start_link({local, rc_cqerl}, ?MODULE, Args, []).

init(Args) ->
  self() ! connect,
  process_flag(trap_exit, true),
  Defaults = default_config(),
  ValidArgs = maps:merge(Defaults, Args),
  State = maps:merge(ValidArgs, #{retries => 0, state => disconnected}),
  {ok, State}.

handle_info(connect, #{reconnection := {uniform, Time}} = State) ->
  #{host := Host, port := Port} = State,
  case cqerl:get_client({Host, Port}) of
    {ok, Conn} ->
      {noreply, State#{connection_ref => Conn, state => connected}};
    {error, Reason} ->
      #{retries := Retries} = State,
      lager:info("cassandra connection error: ~p~n", [Reason]),
      erlang:send_after(Time, self(), connect),
      {noreply, State#{retries => Retries + 1, state => disconnected}}
  end;

handle_info(_Msg, State) ->
  {noreply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_call(get_connection, _From,  #{connection_ref := Conn, state := connected} = State) ->
  {Pid, _} = Conn,
  case erlang:process_info(Pid) of
    undefined ->
      #{retries := Retries, reconnection := {uniform, Time}} = State,
      lager:info("Error trying to connect with cassandra~nRetries: ~p~n", [Retries]),
      erlang:send_after(Time, self(), connect),
      {reply, {error, disconnected}, State#{retries => Retries + 1, state => disconnected}};
    _ ->
      {reply, {ok, Conn}, State}
  end;
handle_call(get_connection, _From, State) ->
  {reply, {error, disconnected}, State}.

terminate(Reason, State) ->
  lager:info("cassandra connection server terminate.~nReason: ~p", [Reason]),
  {noreply, State}.

default_config() ->
#{host => "127.0.0.1",
  port => 9042,
  reconnection => {uniform, 100}}.
