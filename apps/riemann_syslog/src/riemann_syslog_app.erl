-module(riemann_syslog_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  {ok, _} = syslog_drain:start_server(riemann_syslog_drain, riemann_syslog:get_env(drain_acceptors, 100),
    [{port, riemann_syslog:get_env(port, 10514)}], []),

  {ok, _} = syslog_drain_ws:start_server(riemann_syslog_drain_ws, riemann_syslog:get_env(ws_drain_acceptors, 100),
    [{port, riemann_syslog:get_env(ws_port, 10515)}], []),

  riemann_syslog_sup:start_link().

stop(_State) ->
    ok.
