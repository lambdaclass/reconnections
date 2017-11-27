-module (cqerl_SUITE).

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
  application:ensure_all_started(cqerl),
  BlockadeContainers = #{
    containers => #{
      cassandra => #{
        image => <<"cassandra">>,
        ports => #{<<"9042">> => 9042}
      }
    }
  },
  Host = "docker.for.mac.localhost",
  Port = 5000,
  Name = "cassandra",
  % Create a cassandra container with Blockaderl
  ok = blockaderl:create(Host, Port, Name, BlockadeContainers),
  % Get the Host name from the erlang container with the reconnections app
  {ok, HostnameList} = inet:gethostname(),
  Hostname = erlang:list_to_binary(HostnameList),
  % Add the erlang container with the container with the service
  ok = blockaderl:add_containers(Host, Port, Name, [Hostname]),
  % Obtain the Ip of the container with the service
  {ok, #{<<"cassandra">> := Ip}} = blockaderl:containers_ips(Host, Port, Name),
  % Start the service through reconnections specifying the host of the service,
  % it has to be named because the cqerl process is used by the library.
  reconnections:start_service(cqerl, #{host => erlang:binary_to_list(Ip), name => rc_cqerl}),
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
  ok = reconnections:stop_service(rc_cqerl),
  ok = blockaderl:delete(Host, Port, Name),
  Config.

%% Test cases

basic(_Config) ->
  {ok, _, Con} = test_utils:try_to_connect(rc_cqerl, 300),
  {ok, _} = cqerl:run_query(Con, "SELECT * FROM system.peers;"),
  ok.

service_down_at_start(Config) ->
  Host = proplists:get_value(host, Config),
  Port = proplists:get_value(port, Config),
  Name = proplists:get_value(name, Config),
  ServiceIp = proplists:get_value(service_ip, Config),

  % Stop service
  ok = reconnections:stop_service(rc_cqerl),
  ok = blockaderl:containers_stop(Host, Port, Name, [<<"cassandra">>]),

  % Try to start the cassandra connector with the stopped service host
  reconnections:start_service(cqerl, #{host => ServiceIp, name => rc_cqerl}),

  % Get state when service is down
  {error, 10, disconnected} = test_utils:try_to_connect(rc_cqerl, 10),

  % restart the service
  ok = blockaderl:containers_start(Host, Port, Name, [<<"cassandra">>]),

  % Get state when network status is ok
  {ok, _, Con} = test_utils:try_to_connect(rc_cqerl, 300),
  {ok, _} = cqerl:run_query(Con, "SELECT * FROM system.peers;"),
  ok.

service_down(Config) ->
  Host = proplists:get_value(host, Config),
  Port = proplists:get_value(port, Config),
  Name = proplists:get_value(name, Config),

  % Get state when network status is ok
  {ok, 0, Con} = test_utils:try_to_connect(rc_cqerl, 100),
  {ok, _} = cqerl:run_query(Con, "SELECT * FROM system.peers;"),

  %% Stop cassandra
  ok = blockaderl:containers_stop(Host, Port, Name, [<<"cassandra">>]),

  % Get state when service is down
  {error, 10, disconnected} = test_utils:try_to_connect(rc_cqerl, 10),

  % restart the service
  ok = blockaderl:containers_start(Host, Port, Name, [<<"cassandra">>]),
  % Reconnections get the connection id automatically
  {ok, _, Con2} = test_utils:try_to_connect(rc_cqerl, 300),
  {ok, _} = cqerl:run_query(Con2, "SELECT * FROM system.peers;"),
  ok.