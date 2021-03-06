-module (riemann_syslog_heroku_metric).

-export ([handle_message/1]).

-define (HOST_NAME, "fs-([^\.]*)-(prod|test).*").

handle_message({ok, Message})->
  Dyno = proplists:get_value(dyno, Message),
  System = proplists:get_value(system, Message),
  message_to_event(Message, Dyno, System);
handle_message(_)->
  [].

message_to_event(Message, <<"router">>, <<"heroku">>)->
  heroku_router_metrics(Message);
message_to_event(Message, <<"web", _/binary>>, <<"heroku">>)->
  heroku_dyno_metrics(Message);
message_to_event(_, _, _)->
  [].


%% Send event for
%%   http response
%%     heroku platform errors
%%     server error
%%     client error
%%     ok
%%   queue
%%   wait
%%   connect
%%   service
%%   bytes
%%   
heroku_dyno_metrics(Message)->
  MessageParts = proplists:get_value(message_parts, Message, []),
  Drain = proplists:get_value(drain, Message, <<"undefined">>),
  Dyno = proplists:get_value(dyno, Message, <<"web.0">>),
  Time = proplists:get_value(timestamp, Message, 0),
  Measure = proplists:get_value(<<"measure">>, MessageParts, <<"unknown">>),
  Val = proplists:get_value(<<"val">>, MessageParts, <<"0">>),
  Metric = binary_to_number(Val),

  AppName = case catch ets:lookup(apps,Drain) of
    [{_,Entry}|_] -> Entry;
    _ -> Drain
  end,

  [[
    {host, <<AppName/binary,".", Dyno/binary>>},
    {time, Time},
    {description, <<Measure/binary," ",Dyno/binary>>},
    {state, <<"ok">>},
    {service, Measure},
    {metric, Metric},
    {ttl, 600},
    {tags, [
      proplists:get_value(<<"units">>, MessageParts),
      proplists:get_value(<<"source">>, MessageParts)
    ]}
  ]].

binary_to_number(Bin)->
  case catch binary_to_float(Bin) of
    N when is_float(N) -> N;
    _ -> case catch binary_to_integer(Bin) of
      N when is_integer(N) -> N;
      _ -> case catch binary_ms_to_integer(Bin) of
        N when is_integer(N) -> N;
        _ -> 0
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

heroku_router_metrics(Message)->
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
  MessageParts = proplists:get_value(message_parts, Message, []),
  Drain = proplists:get_value(drain, Message, <<"">>),
  Method = proplists:get_value(<<"method">>, MessageParts, <<"">>),
  Path = proplists:get_value(<<"path">>, MessageParts, <<"">>),
  AppName = case catch ets:lookup(apps,Drain) of
    [{_,Entry}|_] -> Entry;
    _ ->
      [_,App,AppEnv|_] = re:split(proplists:get_value(<<"host">>, MessageParts), ?HOST_NAME),
      NewEntry = <<AppEnv/binary,".",App/binary>>,
      ets:insert(apps,{Drain,NewEntry}),
      NewEntry
  end,

  [
    {host, <<AppName/binary,".router">>},
    {time, proplists:get_value(timestamp, Message)},
    {description, <<Method/binary," ",Path/binary>>},
    {tags, [
      proplists:get_value(<<"request_id">>, MessageParts),
      proplists:get_value(<<"host">>, MessageParts)
    ]}
  ].

http_status_metric(Event, Message)->
  MessageParts = proplists:get_value(message_parts, Message),
  Event++[
    {state, http_status_state(proplists:get_value(<<"status">>, MessageParts))},
    {service, <<"http req">>},
    {ttl, 600}
  ].
queue_metric(Event, Message)->
  MessageParts = proplists:get_value(message_parts, Message),
  Event++[
    {state, <<"ok">>},
    {service, <<"queue">>},
    {metric, binary_to_number(proplists:get_value(<<"queue">>, MessageParts, <<"0">>))},
    {ttl, 600}
  ].
bytes_metric(Event, Message)->
  MessageParts = proplists:get_value(message_parts, Message),
  Event++[
    {state, <<"ok">>},
    {service, <<"bytes">>},
    {metric, binary_to_number(proplists:get_value(<<"bytes">>, MessageParts, <<"0">>))},
    {ttl, 600}
  ].
service_metric(Event, Message)->
  ms_metric(Event, Message, <<"service">>, {1000, 5000}).
connect_metric(Event, Message)->
  ms_metric(Event, Message, <<"connect">>, {5, 20}).
wait_metric(Event, Message)->
  ms_metric(Event, Message, <<"wait">>, {5, 20}).

ms_metric(Event, Message, Name, {Warning, Error})->
  MessageParts = proplists:get_value(message_parts, Message, []),
  MetricBin = proplists:get_value(Name, MessageParts, <<"0ms">>),
  %% ms = -2
  Metric = binary_to_number(MetricBin),
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
    {ttl, 600}
  ].

http_status_state(<<"5",_/binary>>)->
  <<"critical">>;
http_status_state(<<"4",_/binary>>)->
  <<"warning">>;
http_status_state(_)->
  <<"ok">>.
