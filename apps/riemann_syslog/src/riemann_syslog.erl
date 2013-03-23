%%
%% riemann_syslog.erl
%% riemann_syslog entry point
%%
-module (riemann_syslog).

-export ([start/0, start_link/0, stop/0]).

-export ([send/1]).
-export ([send_folsom_metrics/0]).
-export ([get_env/2]).

start_link() ->
  riemann_syslog_sup:start_link().

start() ->
  ok = application:start(ranch),
  ok = lager:start(),
  ok = application:start(folsom),
  ok = application:start(riemann),
  ok = application:start(riemann_syslog).

stop() ->
  application:stop(riemann_syslog).

send(Events)->
  poolboy:transaction(riemann, fun(Worker) ->
      gen_server:call(Worker, {send, Events})
  end).

send_folsom_metrics()->
  Metrics = folsom_metrics:get_metric_value(events),
  Event = riemann:event([
    {host, <<"riemann-syslog">>},
    {state, <<"ok">>},
    {service, <<"events/sec">>},
    {metric, proplists:get_value(one, Metrics, 0)/60},
    {ttl, 60}
  ]),
  send([Event]).

get_env(Name, Default) ->
  case application:get_env(riemann_syslog, Name) of
    {ok, V} -> V;
    _ -> Default
  end.
