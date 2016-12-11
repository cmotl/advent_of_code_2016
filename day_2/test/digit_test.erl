-module(digit_test).
-include_lib("eunit/include/eunit.hrl").

-import(digit, [decode_instructions/1, decode_instructions/2]).

should_decode_no_instructions_with_default_starting_digit_test() ->
    Instructions = [],
    ?assertEqual("5", decode_instructions(Instructions)).

should_decode_single_digit_from_instructions_test() ->
    Instructions = [[up, left, left]],
    ?assertEqual("1", decode_instructions(Instructions)).

should_decode_multiple_digits_from_instructions_test() ->
    Instructions = [[up, left, left], [right, right, down, down, down]],
    ?assertEqual("19", decode_instructions(Instructions)).

should_decode_no_instructions_test() -> 
    Instructions = [],
    ?assertEqual(5, decode_instructions(5, Instructions)).

should_decode_single_instruction_test() -> 
    Instructions = [up],
    ?assertEqual(2, decode_instructions(5, Instructions)).

should_decode_multiple_instructions_test() -> 
    Instructions = [up, left, left],
    ?assertEqual(1, decode_instructions(5, Instructions)).
