-module (test_utils).

-export ([try_to_connect/2]).

try_to_connect(Driver, Retries) ->
  connect(Driver, Retries, 0).

%%% Private

connect(_, 0, Retries) ->
  {error, Retries, disconnected};
connect(Driver, Remaining, Retries) ->
  case reconnections:get(Driver) of
    {ok, Con} ->
      {ok, Retries, Con};
    {error, Reason} ->
      lager:info("Error during the connection with redis, reason: ~p~n trying again.",
                 [Reason]),
      timer:sleep(100),
      connect(Driver, Remaining - 1, Retries + 1)
  end.