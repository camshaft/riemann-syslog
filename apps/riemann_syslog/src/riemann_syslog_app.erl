-module(riemann_syslog_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  %% App names
  ets:new(apps, [set,public,named_table]),

  {ok, _} = ranch:start_listener(riemann_syslog, riemann_syslog:get_env(nb_acceptors, 100),
          ranch_tcp, [{port, riemann_syslog:get_env(port, 5555)}], riemann_syslog_protocol, []),

  %% Start the frame event handler
  {ok, Pid} = gen_event:start_link({local, frame_man}),

  gen_event:add_handler(Pid, riemann_syslog_frame_handler, []),

  %% We want to pool the gen_server
  application:stop(riemann),

  riemann_syslog_sup:start_link().

stop(_State) ->
    ok.
