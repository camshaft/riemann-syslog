-module(metric_to_riemann).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-export([is_valid/1]).

is_valid(Message)->
  case syslog_pipeline:get_value(message_fields, Message, undefined) of
    undefined ->
      false;
    MessageParts ->
      Measure = syslog_pipeline:get_value(<<"measure">>, MessageParts, undefined),
      Val = syslog_pipeline:get_value(<<"val">>, MessageParts, undefined),
      is_valid(Measure, Val)
  end.

is_valid(undefined, _)->
  false;
is_valid(_, undefined)->
  false;
is_valid(_, _)->
  true.

start_link(Opts) ->
  gen_server:start_link(?MODULE, Opts, []).

init(State) ->
  {ok, State}.

handle_call(_Request, _From, State) ->
  {reply, undef, State}.

handle_cast({handle, Message}, State) ->
  Ranges = syslog_pipeline:get_value(ranges, State, []),
  Events = message_to_riemann(Message, Ranges),
  riemann_syslog:send(Events),
  {noreply, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

message_to_riemann(Message, Ranges)->
  MessageParts = syslog_pipeline:get_value(message_fields, Message, []),
  Hostname = syslog_pipeline:get_value(hostname, Message, <<>>),
  AppName = syslog_pipeline:get_value(app_name, Message, <<>>),
  ProcID = syslog_pipeline:get_value(proc_id, Message, <<>>),
  %% TODO convert to a unix timestamp
  % Time = syslog_pipeline:get_value(timestamp, Message, 0),

  Measure = syslog_pipeline:get_value(<<"measure">>, MessageParts, <<>>),
  Val = syslog_pipeline:get_value(<<"val">>, MessageParts, <<"0">>),
  Metric = riemann_syslog:binary_to_number(Val),

  [[
    {host, <<Hostname/binary,".",AppName/binary,".",ProcID/binary>>},
    % {time, Time},
    {description, <<Measure/binary," ",ProcID/binary>>},
    {state, state(syslog_pipeline:get_value(Measure, Ranges, []), Metric)},
    {service, Measure},
    {metric, Metric},
    {ttl, 30},
    {tags, [
      syslog_pipeline:get_value(<<"units">>, MessageParts, <<>>),
      syslog_pipeline:get_value(<<"source">>, MessageParts, <<>>)
    ]}
  ]].

state([], _)->
  <<"ok">>;
state([{State, Range}|Ranges], Metric) ->
  case Metric of
    M when M > Range ->
      State;
    _ ->
      state(Ranges, Metric)
  end.
