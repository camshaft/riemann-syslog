-module (riemann_syslog_protocol).

-export([start_link/4, init/4]).

start_link(ListenerPid, Socket, Transport, Opts) ->
  Pid = spawn_link(?MODULE, init, [ListenerPid, Socket, Transport, Opts]),
  {ok, Pid}.

init(ListenerPid, Socket, Transport, _Opts = []) ->
  ok = ranch:accept_ack(ListenerPid),
  loop(Socket, Transport, <<>>).

handle(Frame)->
  gen_event:notify(frame_man, {frame, Frame}).

loop(Socket, Transport, Buffer) ->
  case Transport:recv(Socket, 0, 30000) of
    {ok, Data} ->
      {Frames, Buffer2} = riemann_syslog_octet_parser:parse(<< Buffer/binary, Data/binary >>),
      [handle(Frame) || Frame <- Frames],
      loop(Socket, Transport, Buffer2);
    _ ->
      loop(Socket, Transport, Buffer)
  end.
