-module (rc_redis).

-behaviour (gen_server).

-export ([start_link/0]).
-export ([init/1, handle_cast/2, handle_call/3, handle_info/2, terminate/2]).

%%==================================================================================================
%% API functions
%%==================================================================================================

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%==================================================================================================
%% Callbacks
%%==================================================================================================

init([]) ->
  self() ! connect,
  process_flag(trap_exit, true),
  {ok, #{retries => 0}}.

handle_info(connect, State) ->
  try
    {ok, Host}     = application:get_env(redis, host),
    {ok, Port}     = application:get_env(redis, port),
    {ok, Pass}     = application:get_env(redis, password),
    {ok, Database} = application:get_env(redis, database),
    {ok, Timeout}  = application:get_env(redis, reconnect_sleep),
    {ok, C} = eredis:start_link(Host, Port, Database, Pass, Timeout),
    {noreply, State#{connection => C}}
  catch
    Ex:Err ->
      io:format("Catch Error - ~p:~p~n", [Ex, Err]),
      {noreply, State}
  end;
handle_info({'EXIT', _From, Reason}, State = #{retries := Retries}) ->
  io:format("Error trying to connect with redis~nRetries: ~p~nError:~p~n", [Retries, Reason]),
  erlang:send_after(5000, self(), connect),
  {noreply, State#{retries => Retries + 1}};
handle_info(_Msg, State) ->
  {noreply, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_call(_Msg, _From, State) -> {noreply, State}.

terminate(Reason, State) ->
  io:format("Redis connection server terminate.~nReason: ~p", [Reason]),
  {noreply, State}.