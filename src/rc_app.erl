%%%-------------------------------------------------------------------
%% @doc reconnections public API
%% @end
%%%-------------------------------------------------------------------

-module(rc_app).

-behaviour(application).

-export([start/0, start/2, stop/1]).

%%==================================================================================================
%% API functions
%%==================================================================================================
start() ->
  application:ensure_all_started(reconnetcions).

start(_StartType, _StartArgs) ->
  rc_sup:start_link().

stop(_State) ->
  ok.
