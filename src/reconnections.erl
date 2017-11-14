-module(reconnections).

-behaviour(application).

-export([start/0,
         start/2,
         stop/1,
         get/1]).

start() ->
  application:ensure_all_started(reconnections).

start(_StartType, _StartArgs) ->
  rc_sup:start_link().

stop(_State) ->
  ok.

get(ConnName) ->
  gen_server:call(ConnName, get_connection).
