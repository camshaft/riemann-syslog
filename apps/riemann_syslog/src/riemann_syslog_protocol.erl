-module (riemann_syslog_protocol).

-export([start_link/4, init/4]).

start_link(ListenerPid, Socket, Transport, Opts) ->
  Pid = spawn_link(?MODULE, init, [ListenerPid, Socket, Transport, Opts]),
  {ok, Pid}.

init(ListenerPid, Socket, Transport, _Opts = []) ->
  ok = ranch:accept_ack(ListenerPid),
  loop(Socket, Transport, <<>>).

handle(_Frame)->
  %% TODO emit a frame event
  ok.

loop(Socket, Transport, Buffer) ->
  case Transport:recv(Socket, 0, 30000) of
    {ok, Data} ->
      {Frames, Buffer2} = riemann_syslog_parser:parse(<< Buffer/binary, Data/binary >>),
      [handle(Frame) || Frame <- Frames],
      loop(Socket, Transport, Buffer2);
    _ ->
      % Timeout
      loop(Socket, Transport, Buffer)
  end.
