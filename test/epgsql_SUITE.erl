-module (epgsql_SUITE).

-export([ all/0,
          init_per_suite/1,
          end_per_suite/1
        ]).
-export([ basic/1,
          service_down_at_start/1,
          service_down/1
        ]).

all() ->
  [basic, service_down_at_start, service_down].

%% Common test

init_per_suite(Config) ->
  application:ensure_all_started(reconnections),
  application:ensure_all_started(blockaderl),
  BlockadeContainers = #{
    containers => #{
      postgres => #{
        image => <<"postgres">>,
        ports => #{<<"5432">> => 5432},
        environment => #{<<"POSTGRES_PASSWORD">> => <<"example">>}
      }
    }
  },
  Host = "docker.for.mac.localhost",
  Port = 5000,
  Name = "postgres",
  % Create a Postgres container with Blockaderl
  ok = blockaderl:create(Host, Port, Name, BlockadeContainers),

  % Get the Host name from the erlang container with the reconnections app
  {ok, HostnameList} = inet:gethostname(),
  Hostname = erlang:list_to_binary(HostnameList),
  % Add the erlang container with the container with the service
  ok = blockaderl:add_containers(Host, Port, Name, [Hostname]),

  % Obtain the Ip of the container with the service
  {ok, #{<<"postgres">> := Ip}} = blockaderl:containers_ips(Host, Port, Name),
  % Start the service through reconnections specifying the host of the service
  reconnections:start_service(epgsql, #{host => erlang:binary_to_list(Ip)}),
  [{host, Host},
   {port, Port},
   {name, Name},
   {containers, BlockadeContainers},
   {service_ip, erlang:binary_to_list(Ip)},
   {ct_hostname, Hostname} | Config].

end_per_suite(Config) ->
  Host = proplists:get_value(host, Config),
  Port = proplists:get_value(port, Config),
  Name = proplists:get_value(name, Config),
  ok = reconnections:stop_service(epgsql),
  ok = blockaderl:delete(Host, Port, Name),
  Config.

%% Test cases

basic(_Config) ->
  {ok, _, Pid} = test_utils:try_to_connect(epgsql, 100),
  {ok, _, _} = epgsql:squery(Pid, "CREATE TABLE users (age int)"),
  {ok, 0, Pid} = test_utils:try_to_connect(epgsql, 100),
  {ok, _} = epgsql:squery(Pid, "INSERT INTO users (age) VALUES (23)"),
  {ok, 0, Pid} = test_utils:try_to_connect(epgsql, 100),
  {ok, _,[{<<"23">>}]} = epgsql:squery(Pid, "SELECT * FROM users"),
  ok.

service_down_at_start(Config) ->
  Host = proplists:get_value(host, Config),
  Port = proplists:get_value(port, Config),
  Name = proplists:get_value(name, Config),
  ServiceIp = proplists:get_value(service_ip, Config),

  ok = reconnections:stop_service(epgsql),
  % Stop service
  ok = blockaderl:containers_stop(Host, Port, Name, [<<"postgres">>]),

  % Try to start the unexistent service with a wrong host
  reconnections:start_service(epgsql, #{host => ServiceIp}),

  % Get state when service is down
  {error, 10, disconnected} = test_utils:try_to_connect(epgsql, 10),

  % restart the service
  ok = blockaderl:containers_start(Host, Port, Name, [<<"postgres">>]),

  % Get state when network status is ok
  {ok, _, Pid} = test_utils:try_to_connect(epgsql, 100),
  {ok, _,[{<<"23">>}]} = epgsql:squery(Pid, "SELECT * FROM users"),
  ok.

service_down(Config) ->
  Host = proplists:get_value(host, Config),
  Port = proplists:get_value(port, Config),
  Name = proplists:get_value(name, Config),

  % Get state when network status is ok
  {ok, 0, Pid} = test_utils:try_to_connect(epgsql, 100),
  {ok, _,[{<<"23">>}]} = epgsql:squery(Pid, "SELECT * FROM users"),

  %% Stop postgres
  ok = blockaderl:containers_stop(Host, Port, Name, [<<"postgres">>]),

  % Get state when service is down
  {error, 10, disconnected} = test_utils:try_to_connect(epgsql, 10),

  % restart the service
  ok = blockaderl:containers_start(Host, Port, Name, [<<"postgres">>]),
  % Reconnections get the connection id automatically
  {ok, _, Pid2} = test_utils:try_to_connect(epgsql, 100),
  {ok, _,[{<<"23">>}]} = epgsql:squery(Pid2, "SELECT * FROM users"),
  ok.