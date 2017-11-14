-module(rc_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
  Drivers  = application:get_env(reconnections, drivers, []),
  Children = drivers_specs(Drivers),
  {ok, {{one_for_one, 3, 10}, Children}}.

%% Priv

drivers_specs(Drivers) ->
  lists:filtermap(fun(Driver) ->
                    Args = maps:from_list(Driver),
                    Module = driver_module(Args),
                    driver_spec(Module, Args)
                  end, Drivers).

driver_module(#{type := epgsql}) -> rc_epgsql;
driver_module(#{type := eredis}) -> rc_eredis;
driver_module(#{type := Driver}) -> {error, bad_driver_type, Driver};
driver_module(_)                 -> {error, missing_driver}.

driver_spec({error, bad_driver_type, Driver}, _) ->
  lager:error("Wrong driver type: ~p.", [Driver]),
  false;

driver_spec({error, missing_driver}, _) ->
  lager:error("Missing driver type."),
  false;

driver_spec(Module, #{reconnection := {uniform, _}} = Args) ->
  {true, #{ id       => Module,
            start    => {Module, start_link, [Args]},
            restart  => permanent,
            shutdown => infinity,
            type     => worker,
            modules  => [Module]
         }};
driver_spec(_, #{type := DriverType}) ->
  lager:error("Missing valid reconnection type in ~p.", [DriverType]),
  false.