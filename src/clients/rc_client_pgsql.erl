-module (rc_client_pgsql).

-export ([test/1]).

test(Pid) ->
  epgsql:squery(Pid, "CREATE TABLE IF NOT EXISTS test (state VARCHAR (50))"),
  epgsql:squery(Pid, "INSERT INTO test (state) VALUES ('is working')"),
  epgsql:squery(Pid, "SELECT state FROM test").