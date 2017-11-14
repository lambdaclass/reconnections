-module (rc_epgsql).

-behaviour (gen_server).

-export ([start_link/1]).
-export ([init/1, handle_info/2, handle_cast/2, handle_call/3, terminate/2]).

start_link(#{name := Name} = Args) ->
  gen_server:start_link({local, Name}, ?MODULE, Args, []);

start_link(Args) ->
  gen_server:start_link({local, epgsql}, ?MODULE, Args, []).

init(Args) ->
  self() ! connect,
  process_flag(trap_exit, true),
  Defaults = #{host => "localhost",
               username => os:getenv("USER"),
               password => "",
               database => "test_db",
               reconnection => {uniform, 500}},
  ValidArgs = maps:merge(Defaults, Args),
  State = maps:merge(ValidArgs, #{retries => 0, state => disconnected}),
  {ok, State}.

handle_info(connect, #{reconnection := {uniform, Time}} = State) ->
  #{host     := Host,
    username := Username,
    password := Pass,
    database := Database} = State,
  case epgsql:connect(Host, Username, Pass, [{database, Database}, {timeout, Time}]) of
    {ok, Pid} ->
      {noreply, State#{connection_ref => Pid, state => connected}};
    {error, Reason} ->
      lager:info("Postgres connection error: ~p~n", [Reason]),
      {noreply, State#{state => disconnected}}
  end;

handle_info({'EXIT', _From, Reason}, #{reconnection := {uniform, Time}} = State) ->
  #{retries := Retries} = State,
  lager:info("Error trying to connect with postgres~nRetries: ~p~nError:~p~n", [Retries, Reason]),
  erlang:send_after(Time, self(), connect),
  {noreply, State#{retries => Retries + 1, state => disconnected}};

handle_info(_Msg, State) ->
  {noreply, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_call(get_connection, _From, #{state := connected} = State) ->
  {reply, {ok, maps:get(connection_ref, State)}, State};

handle_call(get_connection, _From, #{state := ConState} = State) ->
  {reply, {error, ConState}, State}.

terminate(Reason, State) ->
  lager:info("Postgres connection server terminate.~nReason: ~p", [Reason]),
  {noreply, State}.