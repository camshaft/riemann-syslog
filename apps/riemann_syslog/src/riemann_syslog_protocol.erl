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
  Event = riemann_syslog_msg_parser:parse(Frame),
  Opts = riemann_syslog_heroku_metric:handle_message(Event),
  [riemann:event(Opt) || Opt <- Opts].

loop(Socket, Transport, Buffer) ->
  case Transport:recv(Socket, 0, infinity) of
    {ok, Data} ->
      {Frames, Buffer2} = riemann_syslog_octet_parser:parse(<< Buffer/binary, Data/binary >>),
      Events = [case (catch handle(Frame)) of Evs when is_list(Evs)-> Evs; _ -> [] end || Frame <- Frames],
      riemann_syslog:send(lists:flatten(Events)),
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
