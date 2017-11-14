-module (network_manager).

-export ([connect/1, disconnect/1, fast/1, flaky/1]).

-type drivers() :: reconnections:drivers().

-spec connect(drivers()) -> ok.
connect(eredis) ->
  os:cmd("blockade start bredis"),
  ok;
connect(epgsql) ->
  os:cmd("blockade start bpostgres"),
  ok.

-spec disconnect(drivers()) -> ok.
disconnect(eredis) ->
  os:cmd("blockade stop bredis"),
  ok;
disconnect(epgsql) ->
  os:cmd("blockade stop bpostgres"),
  ok.

-spec fast(drivers()) -> ok.
fast(eredis) ->
  os:cmd("blockade fast bredis"),
  ok;
fast(epgsql) ->
  os:cmd("blockade fast bpostgres"),
  ok.

-spec flaky(drivers()) -> ok.
flaky(eredis) ->
  os:cmd("blockade flaky bredis"),
  ok;
flaky(epgsql) ->
  os:cmd("blockade flaky bredis"),
  ok.