-module (riemann_syslog_msg_parser).

-export ([parse/1]).

-define (HEROKU_RE, "^<[0-9]+>[0-9]? ([^ ]+) (d.[^ ]+) ([^ ]+) ([^ ]+) - - (.*)").
-define (KEY_VALUE_PAIR, "([^= ]*)=(\"[^\"]*\"|[^ \"]*) *").

parse(Frame) ->

  Parts = re:split(Frame, ?HEROKU_RE),
  DateTime = iso8601:parse(lists:nth(2, Parts)),
  Message = re:split(lists:nth(6, Parts), ?KEY_VALUE_PAIR),

  {ok, [
    {timestamp, datetime_to_epoch(DateTime)},
    {drain, lists:nth(3, Parts)},
    {system, lists:nth(4, Parts)},
    {dyno, lists:nth(5, Parts)},
    {message, lists:nth(6, Parts)},
    {message_parts, make_parts(Message)}
  ]}.

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
      make_parts(Rest2, List++[{Key, Value}])
  end.

get_value([])->
  {[]};
get_value([<<>>|Rest])->
  {Rest};
get_value([Value|Rest])->
  {Value, Rest}.

datetime_to_epoch(DateTime)->
  calendar:datetime_to_gregorian_seconds(DateTime) - calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}}).
