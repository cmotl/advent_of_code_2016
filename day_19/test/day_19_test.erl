-module(day_19_test).
-include_lib("eunit/include/eunit.hrl").

part1_integration_test() -> 
    NumberOfElves = 3014387,
    Elves = [ {X,1} || X <- lists:seq(1,NumberOfElves) ],
    FinalElf = white_elephant:exchange_gifts(Elves),
    ?assertEqual({1834471,3014387}, FinalElf).

part2_integration_test() -> 
    %NumberOfElves = 3014387,
    NumberOfElves = 10,
    Elves = [ X || X <- lists:seq(1,NumberOfElves) ],
    FinalElf = white_elephant:exchange_gifts_across_slow(Elves),
    ?assertEqual(1420064, FinalElf).

part2_integration_fast_test() -> 
    NumberOfElves = 3014387,
    FinalElf = white_elephant:exchange_gifts_across_fast(NumberOfElves),
    ?assertEqual(1420064, FinalElf).
