-module(reconnections).

-behaviour(application).

-export([start_service/2,
         start/2,
         stop/1,
         get/1]).

-type drivers() :: eredis | epgsql.

-export_type([drivers/0]).

start(_StartType, _StartArgs) ->
  rc_sup:start_link().

stop(_State) ->
  ok.

-spec get(drivers()) -> {ok, pid()} | {error, disconnected}.
get(ConnName) ->
  gen_server:call(ConnName, get_connection).

start_service(Service, Args) ->
  rc_sup:start_service(Service, Args).
