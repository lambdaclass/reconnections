-module (eredis_SUITE).

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

init_per_suite(Config) ->
  application:ensure_all_started(reconnections),
  application:ensure_all_started(blockaderl),

  BlockadeContainers = #{
    containers => #{
       redis => #{
        image => <<"redis:alpine">>,
        <<"ports">> => #{<<"6379">> => 6379}
      }
    }
  },
  Host = "docker.for.mac.localhost",
  Port = 5000,
  Name = "test",
  % Create a Redis container with Blockaderl
  ok = blockaderl:create(Host, Port, Name, BlockadeContainers),

  % Get the Host name from the erlang container with the reconnections app
  {ok, HostnameList} = inet:gethostname(),
  Hostname = erlang:list_to_binary(HostnameList),
  % Add the erlang container with the container with the service
  ok = blockaderl:add_containers(Host, Port, Name, [Hostname]),

  % Obtain the Ip of the container with the service
  {ok, #{<<"redis">> := Ip}} = blockaderl:containers_ips(Host, Port, Name),
  % Start the service through reconnections specifying the host of the service
  reconnections:start_service(eredis, #{host => erlang:binary_to_list(Ip)}),
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
  ok = reconnections:stop_service(eredis),
  ok = blockaderl:delete(Host, Port, Name),
  Config.

%% Test cases

basic(_Config) ->
  {ok, _, Pid} = test_utils:try_to_connect(eredis, 100),
  {ok, <<"OK">>} = eredis:q(Pid, ["SET", "state", "is working"]),
  {ok, 0, Pid} = test_utils:try_to_connect(eredis, 100),
  {ok, <<"is working">>} = eredis:q(Pid, ["GET", "state"]),
  ok.

service_down_at_start(Config) ->
  Host = proplists:get_value(host, Config),
  Port = proplists:get_value(port, Config),
  Name = proplists:get_value(name, Config),
  ServiceIp = proplists:get_value(service_ip, Config),

  ok = reconnections:stop_service(eredis),
  % Stop service
  ok = blockaderl:containers_stop(Host, Port, Name, [<<"redis">>]),

  % Try to start the unexistent service with a wrong host
  reconnections:start_service(eredis, #{host => ServiceIp}),

  % Get state when service is down
  {error, 10, disconnected} = test_utils:try_to_connect(eredis, 10),

  % restart the service
  ok = blockaderl:containers_start(Host, Port, Name, [<<"redis">>]),

  % Get state when network status is ok
  {ok, _, Pid} = test_utils:try_to_connect(eredis, 100),
  {ok, <<"is working">>} = eredis:q(Pid, ["GET", "state"]),
  ok.

service_down(Config) ->
  Host = proplists:get_value(host, Config),
  Port = proplists:get_value(port, Config),
  Name = proplists:get_value(name, Config),

  %% Get state when network status is ok
  {ok, 0, Pid} = test_utils:try_to_connect(eredis, 100),
  {ok, <<"is working">>} = eredis:q(Pid, ["GET", "state"]),

  %% Stop redis
  ok = blockaderl:containers_stop(Host, Port, Name, [<<"redis">>]),

  %% Get state when service is down
  {error, 10, disconnected} = test_utils:try_to_connect(eredis, 10),

  % restart the service
  ok = blockaderl:containers_start(Host, Port, Name, [<<"redis">>]),
  % Reconnections get the connection id automatically
  {ok, _, Pid2} = test_utils:try_to_connect(eredis, 100),
  {ok, <<"is working">>} = eredis:q(Pid2, ["GET", "state"]),
  ok.
