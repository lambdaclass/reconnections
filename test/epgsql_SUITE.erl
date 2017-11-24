-module (epgsql_SUITE).

-export([ all/0,
          init_per_suite/1,
          end_per_suite/1
        ]).
-export([ basic/1,
          service_down/1
        ]).

all() ->
  [basic, service_down].

init_per_suite(Config) ->
  application:ensure_all_started(reconnections),
  application:ensure_all_started(blockaderl),
  % TODO remove this
  blockaderl:delete("docker.for.mac.localhost", 5000, <<"test">>),

  BlockadeContainers = #{
    containers => #{
      postgres => #{
        image => <<"postgres">>,
        ports => #{<<"5432">> => 5432},
        environment => #{<<"POSTGRES_PASSWORD">> => <<"example">>}
      }
    }
  },
  % Create a Postgres container with Blockaderl
  ok = blockaderl:create(<<"docker.for.mac.localhost">>, 5000, "test", BlockadeContainers),
  % Get the Host name from the erlang container with the reconnections app
  {ok, HostnameList} = inet:gethostname(),
  Hostname = erlang:list_to_binary(HostnameList),
  % Add the erlang container with the container with the service
  ok = blockaderl:add_containers(<<"docker.for.mac.localhost">>, 5000, <<"test">>, [Hostname]),
  % Obtain the Ip of the container with the service
  {ok, #{<<"postgres">> := Ip}} =
    blockaderl:containers_ips(<<"docker.for.mac.localhost">>, 5000, "test"),
  % Start the service through reconnections specifying the host of the service
  reconnections:start_service(epgsql, #{host => erlang:binary_to_list(Ip)}),
  [{host, "docker.for.mac.localhost"},
   {port, 5000},
   {name, "test"},
   {containers, BlockadeContainers},
   {ct_hostname, Hostname}| Config].

end_per_suite(Config) ->
  ok = blockaderl:delete("docker.for.mac.localhost", 5000, <<"test">>),
  Config.

basic(_Config) ->
  {ok, _, Pid} = test_utils:try_to_connect(epgsql, 100),
  {ok, _, _} = epgsql:squery(Pid, "CREATE TABLE users (age int)"),
  {ok, 0, Pid} = test_utils:try_to_connect(epgsql, 100),
  {ok, _} = epgsql:squery(Pid, "INSERT INTO users (age) VALUES (23)"),
  {ok, 0, Pid} = test_utils:try_to_connect(epgsql, 100),
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