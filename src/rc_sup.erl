-module(rc_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
  Epgsql =
    #{ id       => rc_pgsql,
       start    => {rc_pgsql, start_link, []},
       restart  => permanent,
       shutdown => infinity,
       type     => worker,
       modules  => [rc_pgsql]
     },
  Eredis =
    #{ id       => rc_redis,
       start    => {rc_redis, start_link, []},
       restart  => permanent,
       shutdown => infinity,
       type     => worker,
       modules  => [rc_redis]
     },
  Children = [Epgsql, Eredis],
  {ok, {{one_for_one, 3, 10}, Children}}.