-module(reconnections).

-behaviour(application).

-export([ start/2,
          stop/1,
          start_service/2,
          stop_service/1,
          get/1]).

start(_StartType, _StartArgs) ->
  rc_sup:start_link().

stop(_State) ->
  ok.

get(ConnName) ->
  gen_server:call(ConnName, get_connection).

start_service(Service, Args) ->
  rc_sup:start_service(Service, Args).

stop_service(Service) ->
  rc_sup:stop_service(Service).
