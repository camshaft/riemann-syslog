-module (riemann_syslog_msg_parser).

-export ([parse/1]).

-define (HEROKU_RE, "^([A-Za-z0-9_]{3}) ([0-9]+) ([0-9]+):([0-9]+):([0-9]+) ([A-Za-z0-9.-]+) ([A-Za-z]+)\\[([A-Za-z0-9.]+)] *").
-define (KEY_VALUE_PAIR, "([^= ]*)=(\"[^\"]*\"|[^ \"]*) *").

parse(Frame) ->

  Parts = re:split(binary_to_list(Frame), ?HEROKU_RE),

  Message = re:split(binary_to_list(lists:nth(10, Parts)), ?KEY_VALUE_PAIR),

  {ok, {[
    {month, lists:nth(2, Parts)},
    {day, lists:nth(3, Parts)},
    {time, {lists:nth(4, Parts), lists:nth(5, Parts), lists:nth(6, Parts)}},
    {drain, lists:nth(7, Parts)},
    {system, lists:nth(8, Parts)},
    {dyno, lists:nth(9, Parts)},
    {message, lists:nth(10, Parts)},
    {message_parts, make_parts(Message)}
  ]}}.

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
