-module(emulator_test).
-include_lib("eunit/include/eunit.hrl").

-import(emulator, [parse/1, advance_instruction_cache_by/2, execute_instruction/2, execute/1, toggle_instruction/1, toggle_instruction_cache/2]).

should_parse_copy_value_to_register_instruction_test() ->
    ?assertEqual({cpy, 100, d}, parse("cpy 100 d")).

should_parse_copy_register_to_register_instruction_test() ->
    ?assertEqual({cpy, a, d}, parse("cpy a d")),
    ?assertEqual({cpy, b, d}, parse("cpy b d")),
    ?assertEqual({cpy, c, d}, parse("cpy c d")),
    ?assertEqual({cpy, d, d}, parse("cpy d d")).

should_parse_inc_instruction_test() ->
    ?assertEqual({inc, a}, parse("inc a")),
    ?assertEqual({inc, b}, parse("inc b")).

should_parse_dec_instruction_test() ->
    ?assertEqual({dec, a}, parse("dec a")),
    ?assertEqual({dec, b}, parse("dec b")).

should_parse_jnz_instruction_test() ->
    ?assertEqual({jnz, a, -2}, parse("jnz a -2")),
    ?assertEqual({jnz, 2, a}, parse("jnz 2 a")),
    ?assertEqual({jnz, 1, -2}, parse("jnz 1 -2")).

should_parse_tgl_instruction_test() ->
    ?assertEqual({tgl, c}, parse("tgl c")).


should_toggle_single_argument_instructions_test() ->
    ?assertEqual({dec, a}, toggle_instruction({inc, a})),
    ?assertEqual({inc, b}, toggle_instruction({dec, b})),
    ?assertEqual({inc, c}, toggle_instruction({tgl, c})).

should_toggle_two_argument_instructions_test() ->
    ?assertEqual({cpy, 1, 2}, toggle_instruction({jnz, 1, 2})),
    ?assertEqual({jnz, 2, a}, toggle_instruction({cpy, 2, a})).



should_advance_instruction_cache_test() ->
    InitialInstructionCache = {[], [1,2,3,4]},
    ExpectedInstructionCache = {[1], [2,3,4]},
    ActualInstructionCache = advance_instruction_cache_by(InitialInstructionCache, 1),
    ?assertEqual(ExpectedInstructionCache, ActualInstructionCache).

should_advance_instruction_cache_by_count_test() ->
    InitialInstructionCache = {[], [1,2,3,4]},
    ExpectedInstructionCache = {[2,1], [3,4]},
    ActualInstructionCache = advance_instruction_cache_by(InitialInstructionCache, 2),
    ?assertEqual(ExpectedInstructionCache, ActualInstructionCache).

should_rewind_instruction_cache_test() ->
    InitialInstructionCache = {[4,3,2,1], []},
    ExpectedInstructionCache = {[3,2,1], [4]},
    ActualInstructionCache = advance_instruction_cache_by(InitialInstructionCache, -1),
    ?assertEqual(ExpectedInstructionCache, ActualInstructionCache).
    
should_rewind_instruction_cache_by_count_test() ->
    InitialInstructionCache = {[4,3,2,1], []},
    ExpectedInstructionCache = {[2,1], [3,4]},
    ActualInstructionCache = advance_instruction_cache_by(InitialInstructionCache, -2),
    ?assertEqual(ExpectedInstructionCache, ActualInstructionCache).


    


should_toggle_current_instruction_cache_test() ->
    InitialInstructionCache = {[], [{inc, a},2,3,4]},
    ExpectedInstructionCache = {[], [{dec, a}, 2,3,4]},
    ActualInstructionCache = toggle_instruction_cache(InitialInstructionCache, 0),
    ?assertEqual(ExpectedInstructionCache, ActualInstructionCache).

should_toggle_future_instruction_cache_test() ->
    InitialInstructionCache = {[], [1, {inc, a}, 3,4]},
    ExpectedInstructionCache = {[], [1, {dec, a},3,4]},
    ActualInstructionCache = toggle_instruction_cache(InitialInstructionCache, 1),
    ?assertEqual(ExpectedInstructionCache, ActualInstructionCache).

should_toggle_past_instruction_cache_test() ->
    InitialInstructionCache = {[3, {inc,a}, 1], [4]},
    ExpectedInstructionCache = {[3, {dec,a}, 1], [4]},
    ActualInstructionCache = toggle_instruction_cache(InitialInstructionCache, -2),
    ?assertEqual(ExpectedInstructionCache, ActualInstructionCache).

should_toggle_future_instruction_beyond_cache_test() ->
    InitialInstructionCache = {[], [1, {inc, a}, 3,4]},
    ExpectedInstructionCache = {[], [1, {inc, a},3,4]},
    ActualInstructionCache = toggle_instruction_cache(InitialInstructionCache, 5),
    ?assertEqual(ExpectedInstructionCache, ActualInstructionCache).

should_toggle_past_instruction_beyond_cache_test() ->
    InitialInstructionCache = {[3, {inc,a}, 1], [4]},
    ExpectedInstructionCache = {[3, {inc,a}, 1], [4]},
    ActualInstructionCache = toggle_instruction_cache(InitialInstructionCache, -5),
    ?assertEqual(ExpectedInstructionCache, ActualInstructionCache).



should_execute_inc_instruction_test() ->
    InitialState = {#{a => 0}, {[], [1]}},
    Instruction = {inc, a},
    ExpectedState = {#{a => 1}, {[1],[]}},
    ActualState = execute_instruction(Instruction, InitialState),
    ?assertEqual(ExpectedState, ActualState).

should_execute_dec_instruction_test() ->
    InitialState = {#{a => 2}, {[],[1]}},
    Instruction = {dec, a},
    ExpectedState = {#{a => 1}, {[1], []}},
    ActualState = execute_instruction(Instruction, InitialState),
    ?assertEqual(ExpectedState, ActualState).

should_execute_cpy_value_instruction_test() ->
    InitialState = {#{a => 0}, {[], [1]}},
    Instruction = {cpy, 1, a},
    ExpectedState = {#{a => 1}, {[1], []}},
    ActualState = execute_instruction(Instruction, InitialState),
    ?assertEqual(ExpectedState, ActualState).

should_execute_cpy_register_instruction_test() ->
    InitialState = {#{a => 0, b => 1},{[],[1]}},
    Instruction = {cpy, a, b},
    ExpectedState = {#{a => 0, b => 0},{[1],[]}},
    ActualState = execute_instruction(Instruction, InitialState),
    ?assertEqual(ExpectedState, ActualState).

should_execute_invalid_cpy_instruction_test() ->
    InitialState = {#{a => 0, b => 1},{[],[1]}},
    Instruction = {cpy, 1, 2},
    ExpectedState = {#{a => 0, b => 1},{[1],[]}},
    ActualState = execute_instruction(Instruction, InitialState),
    ?assertEqual(ExpectedState, ActualState).

should_execute_jnz_instruction_on_non_jump_condition_test() ->
    Registers = #{},
    InstructionCache = {[3,2,1],[4,5,6]},
    InitialState = {Registers, InstructionCache},
    ExpectedState = {Registers, {[4,3,2,1],[5,6]}},
    Instruction = {jnz, 0, 1},
    ActualState = execute_instruction(Instruction, InitialState),
    ?assertEqual(ExpectedState, ActualState).

should_execute_jnz_instruction_on_jump_condition_test() ->
    Registers = #{},
    InstructionCache = {[3,2,1],[4,5,6]},
    InitialState = {Registers, InstructionCache},
    ExpectedState = {Registers, {[1],[2,3,4,5,6]}},
    Instruction = {jnz, 1, -2},
    ActualState = execute_instruction(Instruction, InitialState),
    ?assertEqual(ExpectedState, ActualState).

should_execute_jnz_instruction_on_non_jump_condition_in_register_test() ->
    Registers = #{a => 0},
    InstructionCache = {[3,2,1],[4,5,6]},
    InitialState = {Registers, InstructionCache},
    ExpectedState = {Registers, {[4,3,2,1],[5,6]}},
    Instruction = {jnz, a, 1},
    ActualState = execute_instruction(Instruction, InitialState),
    ?assertEqual(ExpectedState, ActualState).

should_execute_jnz_instruction_on_jump_condition_in_register_test() ->
    Registers = #{a => 1},
    InstructionCache = {[3,2,1],[4,5,6]},
    InitialState = {Registers, InstructionCache},
    ExpectedState = {Registers, {[1],[2,3,4,5,6]}},
    Instruction = {jnz, a, -2},
    ActualState = execute_instruction(Instruction, InitialState),
    ?assertEqual(ExpectedState, ActualState).

should_execute_jnz_instruction_on_delta_in_register_test() ->
    Registers = #{a => -2},
    InstructionCache = {[3,2,1],[4,5,6]},
    InitialState = {Registers, InstructionCache},
    ExpectedState = {Registers, {[1],[2,3,4,5,6]}},
    Instruction = {jnz, 1, a},
    ActualState = execute_instruction(Instruction, InitialState),
    ?assertEqual(ExpectedState, ActualState).

should_execute_single_instruction_test() -> 
    Registers = #{a => 0},
    InstructionCache = {[], [{inc, a}]},
    InitialState = {Registers, InstructionCache},
    ExpectedState = {#{a => 1}, {[{inc, a}], []}},
    ActualState = execute(InitialState),
    ?assertEqual(ExpectedState, ActualState).

should_execute_multiple_instruction_without_jump_test() -> 
    Registers = #{a => 0, b => 0},
    InstructionCache = {[], [{inc, a}, {inc, b}]},
    InitialState = {Registers, InstructionCache},
    ExpectedState = {#{a => 1, b => 1}, {[{inc, b}, {inc, a}], []}},
    ActualState = execute(InitialState),
    ?assertEqual(ExpectedState, ActualState).

should_execute_multiple_instruction_with_jump_test() -> 
    Registers = #{a => 0, b => 0},
    InstructionCache = {[], [{cpy, 2, b}, {inc, a}, {dec, b}, {jnz, b, -2}]},
    InitialState = {Registers, InstructionCache},
    ExpectedState = {#{a => 2, b => 0}, {[{jnz, b, -2}, {dec, b}, {inc, a}, {cpy, 2, b}], []}},
    ActualState = execute(InitialState),
    ?assertEqual(ExpectedState, ActualState).

should_execute_multiple_instructions_test() ->
    Instructions = [
        {cpy, 2, a},
        {tgl, a},
        {tgl, a},
        {tgl, a},
        {cpy, 1, a},
        {dec, a},
        {dec, a}
    ],
    {Registers, _InstructionCache} = execute( 
        {#{a => 0}, 
        {[], Instructions}}
    ),
    #{a := Value } = Registers,
    ?assertEqual(3, Value).
