-module (riemann_syslog_frame_handler).
-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3,
terminate/2]).

init(_Args) ->
  {ok, []}.

handle_event({frame, Frame}, State) ->
  try riemann_syslog_msg_parser:parse(Frame) of
    {ok, Message} ->
      io:format("***Message*** ~p~n", [Message]),
      ok
  catch
    _ ->
      ok
  end,
  {ok, State};
handle_event(_, State)->
  {ok, State}.

handle_call(_, State) ->
  {ok, ok, State}.
 
handle_info(_, State) ->
  {ok, State}.
 
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Args, _State) ->
  ok.
