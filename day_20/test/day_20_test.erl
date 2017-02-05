-module(day_20_test).
-include_lib("eunit/include/eunit.hrl").

part1_integration_test() -> 
	{ok, Binary} = file:read_file("input.txt"),
    RawLines = binary:bin_to_list(Binary),
    SplitLines = string:tokens(RawLines, "\n"),
    Ranges = lists:map(fun blacklist:parse/1, SplitLines),
    SortedRanges = lists:sort(Ranges),
    {_,LowestBlockedRange} = lists:foldl(fun blacklist:lowest_blocked_range/2, nil, SortedRanges),
    ?assertEqual(17348574, LowestBlockedRange+1).

part2_integration_test() -> 
	{ok, Binary} = file:read_file("input.txt"),
    RawLines = binary:bin_to_list(Binary),
    SplitLines = string:tokens(RawLines, "\n"),
    Ranges = lists:map(fun blacklist:parse/1, SplitLines),
    ?assertEqual(958, length(Ranges)).
