-module (eredis_SUITE).

-export([ all/0,
          init_per_suite/1,
          end_per_suite/1,
          init_per_testcase/2,
          end_per_testcase/2]).
-export([ basic/1,
          intermitent/1,
          flaky/1]).

-type config() :: [{atom(), term()}].

%%% Common test

-spec all() -> [atom()].
all() -> [Fun || {Fun, 1} <- module_info(exports), Fun =/= module_info].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  % {ok, RedisIp} = blockade:get_ip(redis),
  application:ensure_all_started(reconnections),
  Config.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  Config.

-spec init_per_testcase(atom(), config()) -> config().
init_per_testcase(flaky, Config) ->
  network_manager:flaky(eredis),
  Config;

init_per_testcase(_, Config) ->
  Config.

-spec end_per_testcase(atom(), config()) -> config().
end_per_testcase(flaky, Config) ->
  network_manager:fast(eredis),
  Config;

end_per_testcase(_, Config) ->
  Config.

%%% Test cases

-spec basic(config()) -> ok.
basic(_Config) ->
  {ok, Pid} = reconnections:get(eredis),
  {ok, <<"OK">>} = eredis:q(Pid, ["SET", "state", "is working"]),
  {ok, Pid} = reconnections:get(eredis),
  {ok, <<"is working">>} = eredis:q(Pid, ["GET", "state"]),
  ok.

-spec intermitent(config()) -> ok.
intermitent(_Config) ->
  % Get state when network status is ok
  {ok, Pid} = reconnections:get(eredis),
  {ok, <<"is working">>} = eredis:q(Pid, ["GET", "state"]),
  % Get state when network is disconnected
  network_manager:disconnect(eredis),
  timer:sleep(5000),
  {error, disconnected} = reconnections:get(eredis),
  % Network is ok againg and we obtain the connection
  network_manager:connect(eredis),
  {ok, Pid} = reconnections:get(eredis),
  {ok, <<"is working">>} = eredis:q(Pid, ["GET", "state"]),
  ok.

-spec flaky(config()) -> ok.
flaky(_Config) ->
  {ok, _, Pid} = test_utils:try_to_connect(eredis, 5),
  {ok, <<"OK">>} = eredis:q(Pid, ["SET", "state", "is working"]),
  {ok, <<"is working">>} = eredis:q(Pid, ["GET", "state"]),
  ok.