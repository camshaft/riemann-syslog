-module(heroku_router_to_riemann).
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
  AppName = syslog_pipeline:get_value(app_name, Message, undefined),
  ProcID = syslog_pipeline:get_value(proc_id, Message, undefined),
  is_valid(AppName, ProcID).

is_valid(<<"heroku">>, <<"router">>)->
  true;
is_valid(_, _)->
  false.

start_link(Opts) ->
  gen_server:start_link(?MODULE, Opts, []).

init(State) ->
  {ok, State}.

handle_call(_Request, _From, State) ->
  {reply, undef, State}.

handle_cast({handle, Message}, State) ->
  Events = message_to_riemann(Message),
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


message_to_riemann(Message)->
  GenericEvent = generic_router_event(Message),
  [
    http_status_metric(GenericEvent, Message),
    queue_metric(GenericEvent, Message),
    bytes_metric(GenericEvent, Message),
    service_metric(GenericEvent, Message),
    connect_metric(GenericEvent, Message),
    wait_metric(GenericEvent, Message)
  ].

generic_router_event(Message)->
  MessageParts = syslog_pipeline:get_value(message_parts, Message, []),
  % Drain = syslog_pipeline:get_value(drain, Message, <<"">>),
  %% TODO convert to unix timestamp
  % Time = syslog_pipeline:get_value(timestamp, Message, undefined),
  Method = syslog_pipeline:get_value(<<"method">>, MessageParts, <<"">>),
  Path = syslog_pipeline:get_value(<<"path">>, MessageParts, <<"">>),
  AppName = syslog_pipeline:get_value(<<"host">>, MessageParts, <<>>),

  [
    {host, <<AppName/binary,".router">>},
    % {time, Time},
    {description, <<Method/binary," ",Path/binary>>},
    {tags, [
      syslog_pipeline:get_value(<<"request_id">>, MessageParts, <<>>),
      syslog_pipeline:get_value(<<"host">>, MessageParts, <<>>)
    ]}
  ].

http_status_metric(Event, Message)->
  MessageParts = syslog_pipeline:get_value(message_parts, Message, []),
  Event++[
    {state, http_status_state(syslog_pipeline:get_value(<<"status">>, MessageParts, <<"ok">>))},
    {service, <<"http req">>},
    {ttl, 60}
  ].
queue_metric(Event, Message)->
  MessageParts = syslog_pipeline:get_value(message_parts, Message, []),
  Event++[
    {state, <<"ok">>},
    {service, <<"queue">>},
    {metric, riemann_syslog:binary_to_number(syslog_pipeline:get_value(<<"queue">>, MessageParts, <<"0">>))},
    {ttl, 60}
  ].
bytes_metric(Event, Message)->
  MessageParts = syslog_pipeline:get_value(message_parts, Message, []),
  Event++[
    {state, <<"ok">>},
    {service, <<"bytes">>},
    {metric, riemann_syslog:binary_to_number(syslog_pipeline:get_value(<<"bytes">>, MessageParts, <<"0">>))},
    {ttl, 60}
  ].
service_metric(Event, Message)->
  ms_metric(Event, Message, <<"service">>, {1000, 5000}).
connect_metric(Event, Message)->
  ms_metric(Event, Message, <<"connect">>, {5, 20}).
wait_metric(Event, Message)->
  ms_metric(Event, Message, <<"wait">>, {5, 20}).

ms_metric(Event, Message, Name, {Warning, Error})->
  MessageParts = syslog_pipeline:get_value(message_parts, Message, []),
  MetricBin = syslog_pipeline:get_value(Name, MessageParts, <<"0ms">>),
  %% ms = -2
  Metric = riemann_syslog:binary_to_number(MetricBin),
  State = case Metric of
    M when M > Error ->
      <<"critical">>;
    M when M > Warning ->
      <<"warning">>;
    _ ->
      <<"ok">>
  end,
  Event++[
    {state, State},
    {service, Name},
    {metric, Metric},
    {ttl, 60}
  ].

http_status_state(<<"5",_/binary>>)->
  <<"critical">>;
http_status_state(<<"4",_/binary>>)->
  <<"warning">>;
http_status_state(_)->
  <<"ok">>.

