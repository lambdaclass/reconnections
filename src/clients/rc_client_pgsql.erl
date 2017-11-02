-module (rc_client_pgsql).

-export ([create_db/1, insert/1, select/1]).

create_db(Pid) ->
  epgsql:squery(Pid, "CREATE TABLE users (age int)").

insert(Pid) ->
  epgsql:squery(Pid, "INSERT INTO users (age) VALUES (23)").

select(Pid) ->
  epgsql:squery(Pid, "SELECT * FROM users").