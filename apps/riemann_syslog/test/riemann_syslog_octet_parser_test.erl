-module (riemann_syslog_octet_parser_test).

-include_lib("eunit/include/eunit.hrl").

-define (TESTS, [
  {<<"4 test">>, [<<"test">>], <<>>},
  {<<"4 test10 testing123">>, [<<"testing123">>, <<"test">>], <<>>},
  {<<"testing4 test">>, [<<"test">>], <<>>},
  {<<"        4 test">>, [<<"test">>], <<>>},
  {<<"4 test3">>, [<<"test">>], <<"3">>},
  {<<"10 test">>, [], <<"10 test">>},
  {<<"10 test\n12345">>, [<<"test\n12345">>], <<>>},
  {<<"5 which10 test">>, [<<"which">>], <<"10 test">>},
  {<<"lkjahsdfp98y q3riuh asdf80r qouitrh ohjasdf098u q40934r 09as oasdf0[9u qh asdf[09u areto hasdk 08u as0df 09u as">>, [], <<>>},
  {<<"8 q3riuh asdf80r ">>, [], <<>>},
  {<<"1234567 kljasdflkjasdf">>, [], <<>>},
  {<<"182 <40>1 2013-03-21T22:52:26+00:00 d.de02fad5-ca75-4863-8d0a-de58404f9225 heroku web.1 - - source=heroku.6041702.web.1.dabb0da6-d9d5-4627-a299-0b218adf1d3e measure=load_avg_5m val=0.00\n183 <40>1 2013-03-21T22:52:26+00:00 d.de02fad5-ca75-4863-8d0a-de58404f9225 heroku web.1 - - source=heroku.6041702.web.1.dabb0da6-d9d5-4627-a299-0b218adf1d3e measure=load_avg_15m val=0.00\n">>,
  [<<"<40>1 2013-03-21T22:52:26+00:00 d.de02fad5-ca75-4863-8d0a-de58404f9225 heroku web.1 - - source=heroku.6041702.web.1.dabb0da6-d9d5-4627-a299-0b218adf1d3e measure=load_avg_15m val=0.00\n">>,
   <<"<40>1 2013-03-21T22:52:26+00:00 d.de02fad5-ca75-4863-8d0a-de58404f9225 heroku web.1 - - source=heroku.6041702.web.1.dabb0da6-d9d5-4627-a299-0b218adf1d3e measure=load_avg_5m val=0.00\n">>], <<>>}
]).

parser_test_()->
  [fun() -> run_test(Test) end || Test <- ?TESTS].

run_test({Buffer, ExpectedFrames, ExpectedRest})->
  {Frames, Rest} = riemann_syslog_octet_parser:parse(Buffer),
  ?assertEqual(ExpectedFrames, Frames),
  ?assertEqual(ExpectedRest, Rest).
