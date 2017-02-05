-module(day_20_test).
-include_lib("eunit/include/eunit.hrl").

part1_integration_test() -> 
	{ok, Binary} = file:read_file("input.txt"),
    RawLines = binary:bin_to_list(Binary),
    SplitLines = string:tokens(RawLines, "\n"),
    Ranges = lists:map(fun blacklist:parse/1, SplitLines),
    SortedRanges = lists:sort(Ranges),
    [{_,UpperRange}|_] = blacklist:combine_sorted_ranges(SortedRanges),
    ?assertEqual(17348574, UpperRange+1).

part2_integration_test() -> 
	{ok, Binary} = file:read_file("input.txt"),
    RawLines = binary:bin_to_list(Binary),
    SplitLines = string:tokens(RawLines, "\n"),
    Ranges = lists:map(fun blacklist:parse/1, SplitLines),
    %Ranges = [{5,8},{0,2},{4,7}],
    SortedRanges = lists:sort(Ranges),
    CombinedRanges = blacklist:combine_sorted_ranges(SortedRanges),
    BlockedIps = lists:foldl(fun({L,H}, Sum) -> Sum + H - L + 1 end, 0, CombinedRanges),
    ?assertEqual(104, 4294967296 - BlockedIps).
