-module(day_24_test).
-include_lib("eunit/include/eunit.hrl").

part1_integration_test() -> 
	{ok, Binary} = file:read_file("input.txt"),
    RawLines = binary:bin_to_list(Binary),
    SplitLines = string:tokens(RawLines, "\n"),
    ?assertEqual(39, length(SplitLines)).

part2_integration_test() -> 
	{ok, Binary} = file:read_file("input.txt"),
    RawLines = binary:bin_to_list(Binary),
    SplitLines = string:tokens(RawLines, "\n"),
    ?assertEqual(39, length(SplitLines)).
