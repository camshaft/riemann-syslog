%%
%% riemann_syslog.erl
%% riemann_syslog entry point
%%
-module (riemann_syslog).

-export ([start/0, start_link/0, stop/0]).

start_link() ->
  riemann_syslog_sup:start_link().

start() ->
  ok = application:start(ranch),
  ok = application:start(riemann_syslog).

stop() ->
  application:stop(riemann_syslog).
