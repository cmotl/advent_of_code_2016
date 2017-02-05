-module(blacklist).
-export([parse/1,combine_sorted_ranges/1]).

parse(Range) ->
    [Low, High] = re:split(Range, "-", [{return, list}]),
    {list_to_integer(Low), list_to_integer(High)}.


combine_sorted_ranges([], FinalRange, CombinedRanges) -> lists:reverse([FinalRange|CombinedRanges]);
combine_sorted_ranges([{Low, High}|Rest], {PreviousLow, PreviousHigh}, CombinedRanges) when (Low > PreviousHigh+1 ) ->
    NextRange = {PreviousLow, PreviousHigh},
    combine_sorted_ranges(Rest, {Low,High}, [NextRange|CombinedRanges]);
combine_sorted_ranges([{_Low, High}|Rest], {PreviousLow, PreviousHigh}, CombinedRanges) ->
    combine_sorted_ranges(Rest, {PreviousLow, max(High, PreviousHigh)}, CombinedRanges).

combine_sorted_ranges([]) -> [];
combine_sorted_ranges([FirstRange|Ranges]) -> 
    combine_sorted_ranges(Ranges, FirstRange, []).
