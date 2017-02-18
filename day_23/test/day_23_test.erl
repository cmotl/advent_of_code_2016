-module(day_23_test).
-include_lib("eunit/include/eunit.hrl").

part1_integration_test() -> 
	{ok, Binary} = file:read_file("input.txt"),
    RawLines = binary:bin_to_list(Binary),
    SplitLines = string:tokens(RawLines, "\n"),
    Instructions = lists:map(fun emulator:parse/1, SplitLines),
    {Registers, _InstructionCache} = emulator:execute( 
        {#{a => 7, b => 0, c => 0, d => 0}, 
        {[], Instructions}}
    ),
    #{a := Value } = Registers,
    ?assertEqual(11340, Value).

part2_integration_test() -> 
	{ok, Binary} = file:read_file("input.txt"),
    RawLines = binary:bin_to_list(Binary),
    SplitLines = string:tokens(RawLines, "\n"),
    Instructions = lists:map(fun emulator:parse/1, SplitLines),
    {Registers, _InstructionCache} = emulator:execute( 
        {#{a => 12, b => 0, c => 0, d => 0}, 
        {[], Instructions}}
    ),
    #{a := Value } = Registers,
    ?assertEqual(11340, Value),
    ?assertEqual(26, length(Instructions)).
