-module(blacklist_test).
-include_lib("eunit/include/eunit.hrl").

-import(blacklist, [parse/1, combine_sorted_ranges/1]).

should_parse_blacklist_range_test() ->
    ?assertEqual({0,4}, parse("0-4")).

should_combine_no_ranges_test() ->
    CombinedRanges = combine_sorted_ranges([]), 
    ?assertEqual([],CombinedRanges).

should_combine_single_range_test() ->
    CombinedRanges = combine_sorted_ranges([{0,2}]), 
    ?assertEqual([{0,2}],CombinedRanges).

should_combine_multiple_non_overlapping_ranges_test() ->
    CombinedRanges = combine_sorted_ranges([{0,2}, {4,7}]), 
    ?assertEqual([{0,2}, {4,7}],CombinedRanges).

should_combine_two_overlapping_ranges_test() ->
    CombinedRanges = combine_sorted_ranges([{4,7}, {5,8}]), 
    ?assertEqual([{4,8}],CombinedRanges).

should_combine_multuple_sorted_ranges_test() -> 
    CombinedRanges = combine_sorted_ranges([{0,2},{4,7},{5,8}]), 
    ?assertEqual([{0,2},{4,8}],CombinedRanges).
