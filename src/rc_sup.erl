%%%-------------------------------------------------------------------------------------------------
%% @doc reconnections top level supervisor.
%% @end
%%%-------------------------------------------------------------------------------------------------

-module(rc_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
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
  Children = [Postgres],
  {ok, {{one_for_one, 3, 10}, Children}}.