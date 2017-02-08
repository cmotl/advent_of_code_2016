-module(day_21_test).
-include_lib("eunit/include/eunit.hrl").

part1_integration_test() -> 
	{ok, Binary} = file:read_file("input.txt"),
    RawLines = binary:bin_to_list(Binary),
    SplitLines = string:tokens(RawLines, "\n"),
    Instructions = lists:map(fun scramble:parse/1, SplitLines),
    ScrambledPassword = scramble:execute("abcdefgh", Instructions),
    ?assertEqual("cbeghdaf", ScrambledPassword).

part2_integration_need_to_figure_out_inverse_for_rotate_letter_test() -> 
	{ok, Binary} = file:read_file("input.txt"),
    RawLines = binary:bin_to_list(Binary),
    SplitLines = string:tokens(RawLines, "\n"),
    Instructions = lists:map(fun scramble:parse/1, SplitLines),
    ReverseInstructions = lists:reverse(Instructions),
    UnscramblingInstructions = lists:map(fun scramble:invert_instruction/1, ReverseInstructions),
    UnscrambledPassword = scramble:execute("fbgdceah", UnscramblingInstructions),
    ?assertEqual("bacdefgh", UnscrambledPassword).


perms([]) -> [[]];
perms(L)  -> [[H|T] || H <- L, T <- perms(L--[H])].

part2_integration_brute_force() -> 
	{ok, Binary} = file:read_file("input.txt"),
    RawLines = binary:bin_to_list(Binary),
    SplitLines = string:tokens(RawLines, "\n"),
    Instructions = lists:map(fun scramble:parse/1, SplitLines),
    AllPossibleUnscrambledPasswords = perms("abcdefgh"),
    ScrambledPassword = "fbgdceah", 
    UnscrambledPasswords = lists:filter(fun(X) -> ScrambledPassword == scramble:execute(X, Instructions) end, AllPossibleUnscrambledPasswords), 
    ?assertEqual(["bacdefgh"], UnscrambledPasswords).
