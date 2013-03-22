-module(riemann_syslog_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  {ok, _} = ranch:start_listener(riemann_syslog, get_env(nb_acceptors, 100),
          ranch_tcp, [{port, get_env(port, 5555)}], riemann_syslog_protocol, []),

  %% Start the frame event handler
  {ok, Pid} = gen_event:start_link({local, frame_man}),

  gen_event:add_handler(Pid, riemann_syslog_frame_handler, []),

  riemann_syslog_sup:start_link().

stop(_State) ->
    ok.

get_env(Name, Default) ->
  case application:get_env(riemann_syslog, Name) of
    {ok, V} -> V;
    _ -> Default
  end.