-module (riemann_syslog_msg_parser).

-export ([parse/1]).

-define (HEROKU_RE, "^<[0-9]+>[0-9]? ([^ ]+) (d.[^ ]+) ([^ ]+) ([^ ]+) - - ((.*) ([^= ]*)=(\"[^\"]*\"|[^ \"]*) *)* *").

parse(Frame) ->
  handle_parts(re:split(Frame, ?HEROKU_RE)).

handle_parts([_, Date, Drain, System, Dyno, Message|MessageParts]) ->
  {ok, [
    %% TODO write a lib that parses iso8601 with binary - could potentially double performance of this module
    {timestamp, datetime_to_epoch(iso8601:parse(Date))},
    {drain, Drain},
    {system, System},
    {dyno, Dyno},
    {message, Message},
    {message_parts, make_parts(MessageParts)}
  ]};
handle_parts(_) ->
  {error, badmsg}.


make_parts(Parts)->
  make_parts(Parts, []).

make_parts([], List)->
  List;
make_parts([<<>>|Rest], List)->
  make_parts(Rest, List);
make_parts([Key|Rest], List)->
  case get_value(Rest) of
    {Rest2} ->
      make_parts(Rest2, List);
    {Value, Rest2} ->
      make_parts(Rest2, [{Key, Value}|List])
  end.

get_value([])->
  {[]};
get_value([<<>>|Rest])->
  {Rest};
get_value([Value|Rest])->
  {Value, Rest}.

datetime_to_epoch(DateTime)->
  calendar:datetime_to_gregorian_seconds(DateTime) - calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}}).
