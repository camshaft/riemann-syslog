-module (riemann_syslog_protocol).

-compile([{parse_transform, lager_transform}]).

-export([start_link/4, init/4]).

start_link(ListenerPid, Socket, Transport, Opts) ->
  Pid = spawn_link(?MODULE, init, [ListenerPid, Socket, Transport, Opts]),
  {ok, Pid}.

init(ListenerPid, Socket, Transport, _Opts = []) ->
  ok = ranch:accept_ack(ListenerPid),
  loop(Socket, Transport, <<>>).

handle(Frame)->
  %% This is too slow... :(
  % gen_event:notify(frame_man, {frame, Frame}),
  Event = riemann_syslog_msg_parser:parse(Frame),
  Opts = riemann_syslog_frame_handler:handle_message(Event),
  [riemann:event(Opt) || Opt <- Opts].

loop(Socket, Transport, Buffer) ->
  case Transport:recv(Socket, 0, infinity) of
    {ok, Data} ->
      {Frames, Buffer2} = riemann_syslog_octet_parser:parse(<< Buffer/binary, Data/binary >>),
      Events = [handle(Frame) || Frame <- Frames],
      List = lists:flatten(Events),
      folsom_metrics:notify({events, length(List)}),
      riemann_syslog:send(List),
      loop(Socket, Transport, Buffer2);
    {error, closed}->
      ok;
    {error, timeout}->
      loop(Socket, Transport, Buffer);
    {error, Error} ->
      lager:error("Connection error: ~p~n", [Error]);
    _ ->
      loop(Socket, Transport, Buffer)
  end.
