-module(blacklist).
-export([parse/1, lowest_blocked_range/2]).

parse(Range) ->
    [Low, High] = re:split(Range, "-", [{return, list}]),
    {list_to_integer(Low), list_to_integer(High)}.

lowest_blocked_range(Range, nil) -> Range;
% Indentical Ranges
lowest_blocked_range({Low, High}, {Low, High}) ->
    { Low, High };
% New Range is adjacent lower than Lowest Range 
lowest_blocked_range({NewLow, NewHigh}, {LowestLow, LowestHigh}) when 
    (NewHigh+1 == LowestLow) ->
    {NewLow, LowestHigh};
% New Range is adjacent higher than Lowest Range 
lowest_blocked_range({NewLow, NewHigh}, {LowestLow, LowestHigh}) when 
    (LowestHigh+1 == NewLow) ->
    {LowestLow, NewHigh};
% New Range is higher than Lowest Range
lowest_blocked_range({NewLow, _NewHigh}, {LowestLow, LowestHigh}) when 
    (LowestHigh < NewLow) ->
    {LowestLow, LowestHigh};
% New Range is lower than Lowest Range
lowest_blocked_range({NewLow, NewHigh}, {LowestLow, _LowestHigh}) when 
    (NewHigh < LowestLow) ->
    { NewLow, NewHigh };
% New Range is already contained inside
lowest_blocked_range({NewLow, NewHigh}, {LowestLow, LowestHigh}) when
    ( NewLow > LowestLow ) and (NewHigh < LowestHigh) ->
    { LowestLow, LowestHigh };
% New Range contains Lowest Range completely
lowest_blocked_range({NewLow, NewHigh}, {LowestLow, LowestHigh}) when
    ( NewLow < LowestLow ) and ( NewHigh > LowestHigh ) ->
    { NewLow, NewHigh };
% New Range overlaps on the low side of Lowest Range 
lowest_blocked_range({NewLow, NewHigh}, {LowestLow, LowestHigh}) when
    ( NewHigh >= LowestLow ) and (NewHigh < LowestHigh) ->
    { NewLow, LowestHigh };
% New Range overlaps on the high side of Lowest Range 
lowest_blocked_range({NewLow, NewHigh}, {LowestLow, LowestHigh}) when
    ( LowestHigh >= NewLow ) and (LowestHigh < NewHigh) ->
    { LowestLow, NewHigh }.
