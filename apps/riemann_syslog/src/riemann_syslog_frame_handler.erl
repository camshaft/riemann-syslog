-module (riemann_syslog_frame_handler).
-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3,
terminate/2]).

init(_Args) ->
  {ok, []}.

handle_event({frame, Frame}, State) ->
  spawn(fun() ->
      {ok, Message} = riemann_syslog_msg_parser:parse(Frame),
      Opts = case proplists:get_value(dyno, Message) of
        <<"router">> ->
          heroku_router_metrics(Message);
        <<"web", _/binary>> ->
          heroku_dyno_metrics(Message);
        _ ->
          Drain = proplists:get_value(drain, Message),
          Dyno = proplists:get_value(dyno, Message),
          [[
            {host, <<Drain/binary,".",Dyno/binary>>},
            {time, proplists:get_value(timestamp, Message)},
            {service, proplists:get_value(system, Message)},
            {description, proplists:get_value(message, Message)}
          ]]
      end,
      Events = [riemann:event(Opt) || Opt <- Opts],
      riemann:send(Events)
  end),
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
  MessageParts = proplists:get_value(message_parts, Message),
  Drain = proplists:get_value(drain, Message),
  Dyno = proplists:get_value(dyno, Message),
  Measure = proplists:get_value(<<"measure">>, MessageParts),
  Val = proplists:get_value(<<"val">>, MessageParts),
  Metric = case catch binary_to_float(Val) of
    {'EXIT', {badarg,_}} -> binary_to_integer(Val);
    N -> N
  end,

  [[
    {host, <<Drain/binary,".", Dyno/binary>>},
    {time, proplists:get_value(timestamp, Message)},
    {description, <<Measure/binary," ",Dyno/binary>>},
    {state, <<"ok">>},
    {service, Measure},
    {metric, Metric},
    {ttl, 60},
    {tags, [
      proplists:get_value(<<"units">>, MessageParts),
      proplists:get_value(<<"source">>, MessageParts)
    ]}
  ]].

heroku_router_metrics(Message)->
  GenericEvent = generic_router_event(Message),
  [
    http_status_metric(GenericEvent, Message),
    queue_metric(GenericEvent, Message),
    bytes_metric(GenericEvent, Message)
  ].

generic_router_event(Message)->
  MessageParts = proplists:get_value(message_parts, Message),
  Drain = proplists:get_value(drain, Message),
  Method = proplists:get_value(<<"method">>, MessageParts),
  Path = proplists:get_value(<<"path">>, MessageParts),

  [
    {host, <<Drain/binary,".router">>},
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
    {ttl, 60}
  ].
queue_metric(Event, Message)->
  MessageParts = proplists:get_value(message_parts, Message),
  Event++[
    {state, <<"ok">>},
    {service, <<"queue">>},
    {metric, binary_to_integer(proplists:get_value(<<"queue">>, MessageParts))},
    {ttl, 60}
  ].
bytes_metric(Event, Message)->
  MessageParts = proplists:get_value(message_parts, Message),
  Event++[
    {state, <<"ok">>},
    {service, <<"bytes">>},
    {metric, binary_to_integer(proplists:get_value(<<"bytes">>, MessageParts))},
    {ttl, 60}
  ].

http_status_state(<<"5",_/binary>>)->
  <<"error">>;
http_status_state(<<"4",_/binary>>)->
  <<"client_error">>;
http_status_state(_)->
  <<"ok">>.
