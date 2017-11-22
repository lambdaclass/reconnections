-module(rc_sup).

-behaviour(supervisor).

-export([start_link/0, start_service/2]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_service(Service, Args) ->
  Args2 = Args#{type => Service},
  Module = driver_module(Args2),
  case driver_spec(Module, Args2) of
    {true, ChildSpec} ->
      supervisor:start_child(?MODULE, ChildSpec); %TODO modify this response when fails.
    false ->
      {error, wrong_specification}
  end.

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
driver_module(#{type := Driver}) -> {error, {bad_driver_type, Driver}};
driver_module(_)                 -> {error, missing_driver}.

driver_spec(Module, Args) ->
  {true, #{ id       => Module,
            start    => {Module, start_link, [Args]},
            restart  => permanent,
            shutdown => infinity,
            type     => worker,
            modules  => [Module]
         }}.