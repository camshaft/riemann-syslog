-module(riemann_syslog_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  {ok, _} = ranch:start_listener(riemann_syslog, 1,
          ranch_tcp, [{port, 5555}], riemann_syslog_protocol, []),
  riemann_syslog_sup:start_link().

stop(_State) ->
    ok.
