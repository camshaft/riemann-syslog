-module (riemann_syslog_msg_parser_test).

-include_lib("eunit/include/eunit.hrl").

-define (TESTS, [
  {
    <<"<158>1 2013-03-21T22:55:51+00:00 d.de02fad5-ca75-4863-8d0a-de58404f9225 heroku router - - at=info method=GET path=/ host=fs-home-test.herokuapp.com request_id=755159ef5cfc715185a43e664d0be6c8 fwd=\"216.49.181.254, 204.9.229.1\" dyno=web.1 queue=0 wait=0ms connect=1ms service=364ms status=200 bytes=20946\n">>,
    [
      {timestamp, 1363906551},
      {drain, <<"d.de02fad5-ca75-4863-8d0a-de58404f9225">>},
      {system, <<"heroku">>},
      {dyno, <<"router">>},
      {message, <<"at=info method=GET path=/ host=fs-home-test.herokuapp.com request_id=755159ef5cfc715185a43e664d0be6c8 fwd=\"216.49.181.254, 204.9.229.1\" dyno=web.1 queue=0 wait=0ms connect=1ms service=364ms status=200 bytes=20946">>},
      {message_parts, [
        {<<"at">>, <<"info">>},
        {<<"method">>, <<"GET">>},
        {<<"path">>, <<"/">>},
        {<<"host">>, <<"fs-home-test.herokuapp.com">>},
        {<<"request_id">>, <<"755159ef5cfc715185a43e664d0be6c8">>},
        {<<"fwd">>, <<"\"216.49.181.254, 204.9.229.1\"">>},
        {<<"dyno">>, <<"web.1">>},
        {<<"queue">>, <<"0">>},
        {<<"wait">>, <<"0ms">>},
        {<<"connect">>, <<"1ms">>},
        {<<"service">>, <<"364ms">>},
        {<<"status">>, <<"200">>},
        {<<"bytes">>, <<"20946">>}
      ]}
    ]
  },
  {
    <<"<40>1 2013-03-21T22:52:26+00:00 d.de02fad5-ca75-4863-8d0a-de58404f9225 heroku web.1 - - source=heroku.6041702.web.1.dabb0da6-d9d5-4627-a299-0b218adf1d3e measure=load_avg_1m val=0.00\n">>,
    [
      {timestamp, 1363906346},
      {drain, <<"d.de02fad5-ca75-4863-8d0a-de58404f9225">>},
      {system, <<"heroku">>},
      {dyno, <<"web.1">>},
      {message, <<"source=heroku.6041702.web.1.dabb0da6-d9d5-4627-a299-0b218adf1d3e measure=load_avg_1m val=0.00">>},
      {message_parts, [
        {<<"source">>, <<"heroku.6041702.web.1.dabb0da6-d9d5-4627-a299-0b218adf1d3e">>},
        {<<"measure">>, <<"load_avg_1m">>},
        {<<"val">>, <<"0.00">>}
      ]}
    ]
  }
]).

parser_test_()->
  [fun() -> run_test(Test) end || Test <- ?TESTS].

run_test({Message, Expected})->
  {ok, Msg} = riemann_syslog_msg_parser:parse(Message),
  ?assertEqual(Expected, Msg).
