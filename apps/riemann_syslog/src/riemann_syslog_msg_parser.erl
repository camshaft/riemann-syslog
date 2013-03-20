-module (riemann_syslog_msg_parser).

-export ([parse/1]).

-define (HEROKU_RE, "^([A-Za-z0-9_]{3}) ([0-9]+) ([0-9]+):([0-9]+):([0-9]+) ([A-Za-z0-9.-]+) ([A-Za-z]+)\\[([A-Za-z0-9.]+)] *").
-define (KEY_VALUE_PAIR, "([^= ]*)=(\"[^\"]*\"|[^ \"]*) *").
-define (MONTHS, [
  {<<"Jan">>, 1},
  {<<"Feb">>, 2},
  {<<"Mar">>, 3},
  {<<"Apr">>, 4},
  {<<"May">>, 5},
  {<<"Jun">>, 6},
  {<<"Jul">>, 7},
  {<<"Aug">>, 8},
  {<<"Sep">>, 9},
  {<<"Oct">>, 10},
  {<<"Nov">>, 11},
  {<<"Dec">>, 12}
]).

parse(Frame) ->

  Parts = re:split(Frame, ?HEROKU_RE),

  Message = re:split(lists:nth(10, Parts), ?KEY_VALUE_PAIR),

  Month = proplists:get_value(lists:nth(2, Parts), ?MONTHS, 1),
  Day = binary_to_integer(lists:nth(3, Parts)),
  Time = {
    binary_to_integer(lists:nth(4, Parts)),
    binary_to_integer(lists:nth(5, Parts)),
    binary_to_integer(lists:nth(6, Parts))
  },

  {ok, [
    {month, Month},
    {day, Day},
    {time, Time},
    {timestamp, datetime_to_epoch({{2013, Month, Day},Time})},
    {drain, lists:nth(7, Parts)},
    {system, lists:nth(8, Parts)},
    {dyno, lists:nth(9, Parts)},
    {message, lists:nth(10, Parts)},
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
