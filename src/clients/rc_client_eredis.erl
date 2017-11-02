-module (rc_client_eredis).

-export ([test/1]).

test(Pid) ->
  eredis:q(Pid, ["SET", "state", "is working"]),
  eredis:q(Pid, ["GET", "state"]).