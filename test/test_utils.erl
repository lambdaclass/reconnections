-module (test_utils).

-export ([try_to_connect/2]).

-type drivers() :: reconnections:drivers().

-spec try_to_connect(drivers(), integer()) -> {ok, integer(), pid()} | {error, integer(), timeout}.
try_to_connect(Driver, Retries) ->
  connect(Driver, Retries, 0).

%%% Private

-spec connect(drivers(), integer(), integer()) ->
  {ok, integer(), pid()} | {error, integer(), timeout}.
connect(_, 0, Retries) ->
  {error, Retries, timeout};
connect(Driver, Remaining, Retries) ->
  case reconnections:get(Driver) of
    {ok, Pid} ->
      {ok, Retries, Pid};
    {error, Reason} ->
      lager:info("Error during the connection with redis, reason: ~p~n trying again in a second.",
                 [Reason]),
      timer:sleep(1000),
      connect(Driver, Remaining - 1, Retries + 1)
  end.