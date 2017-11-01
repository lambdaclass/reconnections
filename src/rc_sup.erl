%%%-------------------------------------------------------------------------------------------------
%% @doc reconnections top level supervisor.
%% @end
%%%-------------------------------------------------------------------------------------------------

-module(rc_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

%%==================================================================================================
%% API functions
%%==================================================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%==================================================================================================
%% Supervisor callbacks
%%==================================================================================================

init([]) ->
  Postgres =
    {rc_pgsql, {rc_pgsql, start_link, []},
     permanent, infinity, worker, [rc_pgsql]
    },
  Redis =
    {rc_redis, {rc_redis, start_link, []},
     permanent, infinity, worker, [rc_redis]
    },
  Children = [Postgres, Redis],
  {ok, {{one_for_one, 3, 10}, Children}}.