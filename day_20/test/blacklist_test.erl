-module(blacklist_test).
-include_lib("eunit/include/eunit.hrl").

-import(blacklist, [parse/1, lowest_blocked_range/2]).

should_parse_blacklist_range_test() ->
    ?assertEqual({0,4}, parse("0-4")).


first_blocked_range_is_lowest_blocked_range_test() -> 
    LowestRange = lowest_blocked_range({2, 4}, nil),
    ?assertEqual({2, 4},LowestRange).

initial_lower_blocked_range_beats_higher_blocked_range_test() -> 
    InitialRange = {1, 4},
    HigherRange = {6, 8},
    LowestRange = lowest_blocked_range(HigherRange, InitialRange),
    ?assertEqual(InitialRange,LowestRange).

initial_lower_blocked_range_loses_to_lower_blocked_range_test() -> 
    InitialRange = {6, 8},
    LowerRange = {1, 4},
    LowestRange = lowest_blocked_range(LowerRange, InitialRange),
    ?assertEqual(LowerRange,LowestRange).

initial_lower_blocked_range_merges_with_lower_overlapping_blocked_range_test() -> 
    InitialRange = {6, 8},
    LowerRange = {1, 7},
    LowestRange = lowest_blocked_range(LowerRange, InitialRange),
    ?assertEqual({1, 8},LowestRange).
initial_lower_blocked_range_merges_at_boundary_with_lower_overlapping_blocked_range_test() -> 
    InitialRange = {6, 8},
    LowerRange = {1, 6},
    LowestRange = lowest_blocked_range(LowerRange, InitialRange),
    ?assertEqual({1, 8},LowestRange).

initial_lower_blocked_range_merges_with_higher_overlapping_blocked_range_test() -> 
    InitialRange = {1, 7},
    HigherRange = {6, 8},
    LowestRange = lowest_blocked_range(HigherRange, InitialRange),
    ?assertEqual({1, 8},LowestRange).
initial_lower_blocked_range_merges_at_boundary_with_higher_overlapping_blocked_range_test() -> 
    InitialRange = {1, 6},
    HigherRange = {6, 8},
    LowestRange = lowest_blocked_range(HigherRange, InitialRange),
    ?assertEqual({1, 8},LowestRange).

initial_lower_blocked_range_contains_higher_overlapping_blocked_range_test() -> 
    InitialRange = {1, 8},
    ContainedRange = {6, 7},
    LowestRange = lowest_blocked_range(ContainedRange, InitialRange),
    ?assertEqual({1, 8},LowestRange).

initial_lower_blocked_range_is_contained_in_higher_overlapping_blocked_range_test() -> 
    InitialRange = {6, 7},
    ContainingRange = {1, 8},
    LowestRange = lowest_blocked_range(ContainingRange, InitialRange),
    ?assertEqual({1, 8},LowestRange).

should_combine_ranges_that_are_identical_test() ->
    InitialRange = {6, 7},
    NewRange = {6, 7},
    LowestRange = lowest_blocked_range(NewRange, InitialRange),
    ?assertEqual({6, 7},LowestRange).
    
should_combine_adjacent_ranges_when_new_range_is_lower_test() -> 
    InitialRange = {3,5},
    LowerAdjacentRange = {1,2},
    LowestRange = lowest_blocked_range(LowerAdjacentRange, InitialRange),
    ?assertEqual({1,5},LowestRange).

should_combine_adjacent_ranges_when_new_range_is_higher_test() -> 
    InitialRange = {1,2},
    LowerAdjacentRange = {3,5},
    LowestRange = lowest_blocked_range(LowerAdjacentRange, InitialRange),
    ?assertEqual({1,5},LowestRange).
