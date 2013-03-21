-module (riemann_syslog_msg_parser_test).

-include_lib("eunit/include/eunit.hrl").

-define (TESTS, [
  {
    <<"Mar 12 22:22:20 d.82bd926e-2f86-47ef-ba8b-43e4b4d78412 app[web.1] My Logs">>,
    [
      {month, 3},
      {day, 12},
      {time, {22,22,20}},
      {timestamp, 1363126940},
      {drain, <<"d.82bd926e-2f86-47ef-ba8b-43e4b4d78412">>},
      {system, <<"app">>},
      {dyno, <<"web.1">>},
      {message, <<"My Logs">>},
      {message_parts, []}
    ]
  },
  {
    <<"Mar 12 22:22:20 d.82bd926e-2f86-47ef-ba8b-43e4b4d78412 app[web.1] testing=">>,
    [
      {month, 3},
      {day, 12},
      {time, {22,22,20}},
      {timestamp, 1363126940},
      {drain, <<"d.82bd926e-2f86-47ef-ba8b-43e4b4d78412">>},
      {system, <<"app">>},
      {dyno, <<"web.1">>},
      {message, <<"testing=">>},
      {message_parts, []}
    ]
  },
  {
    <<"Mar 12 22:22:26 d.82bd926e-2f86-47ef-ba8b-43e4b4d78412 heroku[router] at=info method=GET path=/search host=my-search-test.herokuapp.com fwd=\"216.49.181.254\" dyno=web.1 queue=0 wait=0ms connect=1ms service=7ms status=404 bytes=9759">>,
    [
      {month, 3},
      {day, 12},
      {time, {22,22,26}},
      {timestamp, 1363126946},
      {drain, <<"d.82bd926e-2f86-47ef-ba8b-43e4b4d78412">>},
      {system, <<"heroku">>},
      {dyno, <<"router">>},
      {message, <<"at=info method=GET path=/search host=my-search-test.herokuapp.com fwd=\"216.49.181.254\" dyno=web.1 queue=0 wait=0ms connect=1ms service=7ms status=404 bytes=9759">>},
      {message_parts, [
        {<<"at">>, <<"info">>},
        {<<"method">>, <<"GET">>},
        {<<"path">>, <<"/search">>},
        {<<"host">>, <<"my-search-test.herokuapp.com">>},
        {<<"fwd">>, <<"\"216.49.181.254\"">>},
        {<<"dyno">>, <<"web.1">>},
        {<<"queue">>, <<"0">>},
        {<<"wait">>, <<"0ms">>},
        {<<"connect">>, <<"1ms">>},
        {<<"service">>, <<"7ms">>},
        {<<"status">>, <<"404">>},
        {<<"bytes">>, <<"9759">>}
      ]}
    ]
  }
]).

parser_test_()->
  [fun() -> run_test(Test) end || Test <- ?TESTS].

run_test({Message, Expected})->
  {ok, Msg} = riemann_syslog_msg_parser:parse(Message),
  ?assertEqual(Expected, Msg).
