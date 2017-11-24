-module (eredis_SUITE).

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

  blockaderl:delete("docker.for.mac.localhost", 5000, <<"test">>),

  BlockadeContainers = #{
    containers => #{
       redis => #{
        image => <<"redis:alpine">>,
        <<"ports">> => #{<<"6379">> => 6379}
      }
    }
  },
  ok = blockaderl:create(<<"docker.for.mac.localhost">>, 5000, "test", BlockadeContainers),

  {ok, HostnameList} = inet:gethostname(),
  Hostname = erlang:list_to_binary(HostnameList),
  ok = blockaderl:add_containers(<<"docker.for.mac.localhost">>, 5000, <<"test">>, [Hostname]),

  {ok, #{<<"redis">> := Ip }} =
    blockaderl:containers_ips(<<"docker.for.mac.localhost">>, 5000, "test"),
  reconnections:start_service(eredis, #{host => erlang:binary_to_list(Ip) }),
  [{host, "docker.for.mac.localhost"},
   {port, 5000},
   {name, "test"},
   {containers, BlockadeContainers},
   {ct_hostname, Hostname}| Config].

end_per_suite(Config) ->
  ok = blockaderl:delete("docker.for.mac.localhost", 5000, <<"test">>),
  Config.

basic(_Config) ->
  {ok, _, Pid} = test_utils:try_to_connect(eredis, 100),
  {ok, <<"OK">>} = eredis:q(Pid, ["SET", "state", "is working"]),
  {ok, 0, Pid} = test_utils:try_to_connect(eredis, 100),
  {ok, <<"is working">>} = eredis:q(Pid, ["GET", "state"]),
  ok.

service_down(Config) ->
  Host = proplists:get_value(host, Config),
  Port = proplists:get_value(port, Config),
  Name = proplists:get_value(name, Config),
  %% Get state when network status is ok
  {ok, _, Pid} = test_utils:try_to_connect(eredis, 100),
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
