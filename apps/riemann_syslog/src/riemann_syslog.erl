%%
%% riemann_syslog.erl
%% riemann_syslog entry point
%%
-module(riemann_syslog).

-export([start/0, start_link/0, stop/0]).

-export([send/1]).
-export([get_env/2]).
-export([binary_to_number/1]).

start_link() ->
  riemann_syslog_sup:start_link().

start() ->
  ok = application:start(crypto),
  ok = application:start(ranch),
  ok = application:start(cowboy),
  ok = lager:start(),
  ok = application:start(folsom),
  ok = application:start(syslog_pipeline),
  ok = application:start(syslog_drain),
  ok = application:start(syslog_drain_ws),
  ok = application:start(riemann_syslog).

stop() ->
  application:stop(riemann_syslog).

send([])->
  noop;
send(Events)->
  poolboy:transaction(riemann, fun(Worker) ->
    gen_server:call(Worker, {send, [riemann:event(Event) || Event <- Events]})
  end).

get_env(Name, Default) ->
  case application:get_env(riemann_syslog, Name) of
    {ok, V} -> V;
    _ -> Default
  end.

binary_to_number(Bin)->
  case Bin of
    N when is_integer(N) ->
      N;
    _ ->
      case catch binary_to_float(Bin) of
        N when is_float(N) -> N;
        _ -> case catch binary_to_integer(Bin) of
          N when is_integer(N) -> N;
          _ -> case catch binary_ms_to_integer(Bin) of
            N when is_integer(N) -> N;
            _ -> 0
          end
        end
      end
  end.

binary_ms_to_integer(Bin)->
  case byte_size(Bin) of
    Size when Size > 2 ->
      NumberSize = Size-2,
      <<Number:NumberSize/binary, _/binary>> = Bin,
      binary_to_number(Number);
    _ ->
      0
  end.
