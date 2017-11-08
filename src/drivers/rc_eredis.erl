-module (rc_eredis).

-behaviour (gen_server).

-export ([start_link/1]).
-export ([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2]).

start_link(#{name := Name} = Args) ->
  gen_server:start_link({local, Name}, ?MODULE, Args, []);

start_link(Args) ->
  gen_server:start_link({local, eredis}, ?MODULE, Args, []).

init(Args) ->
  self() ! connect,
  process_flag(trap_exit, true),
  Defaults = #{host => "127.0.0.1",
               port => 6379,
               password => "",
               database => 0,
               reconnection => {uniform, 500}},
  ValidArgs = maps:merge(Defaults, Args),
  State = maps:merge(ValidArgs, #{retries => 0, state => disconnected}),
  {ok, State}.

handle_info(connect, #{reconnection := {uniform, Time}} = State) ->
  #{host     := Host,
    port     := Port,
    password := Pass,
    database := Database} = State,
  case eredis:start_link(Host, Port, Database, Pass, Time) of
    {ok, Pid} ->
      {noreply, State#{connection_ref => Pid, state => connected}};
    {error,{connection_error,Reason}} ->
      lager:info("Redis connection error: ~p~n", [Reason]),
      {noreply, State#{state => disconnected}}
  end;

handle_info({'EXIT', _From, Reason}, #{reconnection := {uniform, Time}} = State) ->
  #{retries := Retries} = State,
  lager:info("Error trying to connect with redis~nRetries: ~p~nError:~p~n", [Retries, Reason]),
  erlang:send_after(Time, self(), connect),
  {noreply, State#{retries => Retries + 1, state => disconnected}};

handle_info(_Msg, State) ->
  {noreply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_call(get_connection, _From, #{connection_ref := Pid} = State) ->
  % This is needed because when eredis loses connection does not notify
  case sys:get_state(Pid) of
    {state, _, _, _, _, _, _, undefined, _, _} ->
      {reply, {error,disconnected}, State#{state => disconnected}};
    {state, _, _, _, _, _, _, _, _, _} ->
      {reply, {ok, Pid}, State#{state => connected}}
  end;
handle_call(get_connection, _From, State) ->
  {reply, {error, disconnected}, State}.

terminate(Reason, State) ->
  lager:info("Redis connection server terminate.~nReason: ~p", [Reason]),
  {noreply, State}.