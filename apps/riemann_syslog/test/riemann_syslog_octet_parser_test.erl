-module (riemann_syslog_octet_parser_test).

-include_lib("eunit/include/eunit.hrl").

simple_test()->
  {Frames, Rest} = riemann_syslog_octet_parser:parse(<<"4 test">>),
  ?assertEqual([<<"test">>], Frames),
  ?assertEqual(<<"">>, Rest).

simple_long_test()->
  {Frames, Rest} = riemann_syslog_octet_parser:parse(<<"4 test10 testing123">>),
  ?assertEqual([<<"test">>, <<"testing123">>], Frames),
  ?assertEqual(<<"">>, Rest).

midstream_test()->
  {Frames, Rest} = riemann_syslog_octet_parser:parse(<<"testing4 test">>),
  ?assertEqual([<<"test">>], Frames),
  ?assertEqual(<<"">>, Rest).

spaces_test()->
  {Frames, Rest} = riemann_syslog_octet_parser:parse(<<"        4 test">>),
  ?assertEqual([<<"test">>], Frames),
  ?assertEqual(<<"">>, Rest).

endstream_number_test()->
  {Frames, Rest} = riemann_syslog_octet_parser:parse(<<"4 test3">>),
  ?assertEqual([<<"test">>], Frames),
  ?assertEqual(<<"3">>, Rest).

buffered_test()->
  {Frames, Rest} = riemann_syslog_octet_parser:parse(<<"10 test">>),
  ?assertEqual([], Frames),
  ?assertEqual(<<"10 test">>, Rest).

next_line_test()->
  {Frames, Rest} = riemann_syslog_octet_parser:parse(<<"10 test\n12345">>),
  ?assertEqual([<<"test\n12345">>], Frames),
  ?assertEqual(<<"">>, Rest).

long_buffered_test()->
  {Frames, Rest} = riemann_syslog_octet_parser:parse(<<"5 which10 test">>),
  ?assertEqual([<<"which">>], Frames),
  ?assertEqual(<<"10 test">>, Rest).

garbage_test()->
  {Frames, Rest} = riemann_syslog_octet_parser:parse(<<"lkjahsdfp98y q3riuh asdf80r qouitrh ohjasdf098u q40934r 09as oasdf0[9u qh asdf[09u areto hasdk 08u as0df 09u as">>),
  ?assertEqual([], Frames),
  ?assertEqual(<<"">>, Rest).

garbage2_test()->
  {Frames, Rest} = riemann_syslog_octet_parser:parse(<<"8 q3riuh asdf80r ">>),
  ?assertEqual([], Frames),
  ?assertEqual(<<"">>, Rest).
