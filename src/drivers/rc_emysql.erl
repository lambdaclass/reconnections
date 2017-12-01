-module (rc_emysql).

-behaviour (gen_server).

-export ([start_link/1]).
-export ([init/1, handle_info/2, handle_cast/2, handle_call/3, terminate/2]).

start_link(#{name := Name} = Args) ->
  gen_server:start_link({local, Name}, ?MODULE, Args, []);

start_link(Args) ->
  gen_server:start_link({local, emysql}, ?MODULE, Args, []).

init(Args) ->
  self() ! connect,
  process_flag(trap_exit, true),
  Defaults = default_config(),
  ValidArgs = maps:merge(Defaults, Args),
  State = maps:merge(ValidArgs, #{retries => 0, state => disconnected}),
  {ok, State}.

handle_info(connect, #{reconnection := {uniform, Time}} = State) ->
  #{pool_name := PoolName,
    pool_size := PoolSize,
    username  := Username,
    password  := Pass,
    host      := Host,
    port      := Port,
    database  := Database,
    encoding  := Encoding} = State,
  case emysql:add_pool(PoolName, [{size, PoolSize},
                                  {user, Username},
                                  {password, Pass},
                                  {host, Host},
                                  {port, Port},
                                  {database,Database},
                                  {encoding, Encoding}]) of
    ok ->
      {noreply, State#{connection_ref => PoolName, state => connected}};
    {error, Reason} ->
      lager:info("MySql connection error: ~p~n", [Reason]),
      {noreply, State#{state => disconnected}}
  end;

handle_info({'EXIT', _From, Reason}, #{reconnection := {uniform, Time}} = State) ->
  #{retries := Retries} = State,
  lager:info("Error trying to connect with mysql~nRetries: ~p~nError:~p~n", [Retries, Reason]),
  erlang:send_after(Time, self(), connect),
  {noreply, State#{retries => Retries + 1, state => disconnected}};

handle_info(Msg, State) ->
  lager:info("unknown message ~p", [Msg]),
  {noreply, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_call(get_connection, _From, #{state := connected} = State) ->
  {reply, {ok, maps:get(connection_ref, State)}, State};

handle_call(get_connection, _From, #{state := ConState} = State) ->
  {reply, {error, ConState}, State}.

terminate(Reason, State) ->
  lager:info("MySql connection server terminate.~nReason: ~p", [Reason]),
  {noreply, State}.

default_config() ->
#{pool_name => default_pool,
  pool_size => 1,
  username => "root",
  password => "example",
  host => "localhost",
  port => 3306,
  database => "mysql",
  encoding => utf8,
  reconnection => {uniform, 100}}.