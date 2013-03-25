%%
%% riemann_syslog.erl
%% riemann_syslog entry point
%%
-module (riemann_syslog).

-export ([start/0, start_link/0, stop/0]).

-export ([send/1]).
-export ([get_env/2]).

start_link() ->
  riemann_syslog_sup:start_link().

start() ->
  ok = application:start(ranch),
  ok = lager:start(),
  ok = application:start(riemann_syslog).

stop() ->
  application:stop(riemann_syslog).

send([])->
  noop;
send(Events)->
  poolboy:transaction(riemann, fun(Worker) ->
    gen_server:call(Worker, {send, Events})
  end).

get_env(Name, Default) ->
  case application:get_env(riemann_syslog, Name) of
    {ok, V} -> V;
    _ -> Default
  end.
