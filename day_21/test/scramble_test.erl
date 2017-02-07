-module(scramble_test).
-include_lib("eunit/include/eunit.hrl").

-import(scramble, [parse/1, swap/2, move/2, reverse/2, rotate/2, execute/2, invert_instruction/1]).



should_invert_swap_position_instruction_test() ->
    Instruction = {swap, position, 4, 0},
    ?assertEqual({swap, position, 0, 4}, invert_instruction(Instruction)).
should_invert_swap_letter_instruction_test() ->
    Instruction = {swap, letter, b, d},
    ?assertEqual({swap, letter, d, b}, invert_instruction(Instruction)).
should_invert_rotate_left_instruction_test() ->
    Instruction = {rotate, left, 3},
    ?assertEqual({rotate, right, 3}, invert_instruction(Instruction)).
should_invert_rotate_right_instruction_test() ->
    Instruction = {rotate, right, 3},
    ?assertEqual({rotate, left, 3}, invert_instruction(Instruction)).
should_invert_reverse_instruction_test() ->
    Instruction = {reverse, 1, 3},
    ?assertEqual({reverse, 3, 1}, invert_instruction(Instruction)).
should_invert_move_instruction_test() ->
    Instruction = {move, 1, 3},
    ?assertEqual({move, 3, 1}, invert_instruction(Instruction)).


should_parse_swap_position_instruction_test() ->
    Instruction = "swap position 4 with position 0",
    ?assertEqual({swap, position, 4, 0}, parse(Instruction)).

should_parse_swap_letter_instruction_test() ->
    Instruction = "swap letter b with letter d",
    ?assertEqual({swap, letter, b, d}, parse(Instruction)).

should_parse_rotate_left_instruction_test() ->
    Instruction = "rotate left 6 steps",
    ?assertEqual({rotate, left, 6}, parse(Instruction)).

should_parse_rotate_right_instruction_test() ->
    Instruction = "rotate right 10 steps",
    ?assertEqual({rotate, right, 10}, parse(Instruction)).

should_parse_rotate_based_on_position_of_letter_instruction_test() ->
    Instruction = "rotate based on position of letter c",
    ?assertEqual({rotate, letter, c}, parse(Instruction)).

should_parse_reverse_position_test() ->
    Instruction = "reverse positions 1 through 6",
    ?assertEqual({reverse, 1, 6}, parse(Instruction)).

should_parse_move_position_test() ->
    Instruction = "move position 6 to position 3",
    ?assertEqual({move, 6, 3}, parse(Instruction)).

should_execute_swap_position_instruction_test() ->
    Instruction = {swap, position, 4, 0},
    ?assertEqual("ebcda", swap("abcde", Instruction)).

should_execute_swap_letter_instruction_test() ->
    Instruction = {swap, letter, b, d},
    ?assertEqual("adcbe", swap("abcde", Instruction)).

should_execute_move_position_when_from_is_after_to_test() ->
    Instruction = {move, 3, 1},
    ?assertEqual("adbce", move("abcde", Instruction)).
should_execute_move_position_when_to_is_after_from_test() ->
    Instruction = {move, 1, 3},
    ?assertEqual("acdbe", move("abcde", Instruction)).

should_execute_reverse_instruction_test() ->
    Instruction = {reverse, 1, 4},
    ?assertEqual("aedcbf", reverse("abcdef", Instruction)).
   
should_execute_rotate_right_instruction_test() ->
    Instruction = {rotate, right, 1},
    ?assertEqual("eabcd", rotate("abcde", Instruction)).
    
should_execute_rotate_left_instruction_test() ->
    Instruction = {rotate, left, 1},
    ?assertEqual("bcdea", rotate("abcde", Instruction)).
    
should_execute_rotate_based_on_letter_instruction_test() ->
    Instruction = {rotate, letter, a},
    ?assertEqual("eabcd", rotate("abcde", Instruction)).
should_execute_rotate_based_on_letter_instruction_when_index_is_at_least_4_test() ->
    Instruction = {rotate, letter, e},
    ?assertEqual("eabcd", rotate("abcde", Instruction)).


should_execute_no_instructions_test() ->
    Instructions = [],
    ?assertEqual("abcde", execute("abcde", Instructions)).
should_execute_multiple_instructions_test() ->
    Instructions = [
        {swap, position, 4, 0},
        {swap, letter, d, b},
        {reverse, 0, 4},
        {rotate, left, 1},
        {move, 1, 4},
        {move, 3, 0},
        {rotate, letter, b},
        {rotate, letter, d}
    ],
    ?assertEqual("decab", execute("abcde", Instructions)).
