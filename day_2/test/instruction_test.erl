-module(instruction_test).
-include_lib("eunit/include/eunit.hrl").

-import(instruction, [parse/1]).

should_parse_single_instruction_test() ->
    ?assertEqual(up, parse($U)),
    ?assertEqual(down, parse($D)),
    ?assertEqual(left, parse($L)),
    ?assertEqual(right, parse($R)). 

should_parse_multiple_instructions_test() ->
    ?assertEqual([up, down, left, right], parse("UDLR")).
