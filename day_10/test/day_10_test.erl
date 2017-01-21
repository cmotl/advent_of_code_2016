-module(day_10_test).
-include_lib("eunit/include/eunit.hrl").

part1_integration_test() -> 
    {ok, Binary} = file:read_file("input.txt"),
    RawLines = binary:bin_to_list(Binary),
    SplitLines = string:tokens(RawLines, "\n"),
    ParsedDirections = lists:map(fun factory:parse/1, SplitLines),
    Instructions = lists:filter(fun(X) -> case X of false -> false; _ -> true end end, ParsedDirections),
    PerformedActions = factory:route_instructions_to_bots(Instructions),
    TrimmedActions = lists:map(fun({{_,Bot}, [{_,_,Value1},{_,_,Value2}]}) -> {Bot, Value1, Value2} end, PerformedActions),
    FilteredActions = lists:filter(fun({_Bot, Value1, Value2}) -> ((Value1 == 61) and (Value2 == 17)) or ((Value1 == 17) and (Value2 == 61)) end, TrimmedActions),
    {ComparisonBot, _Chip17, _Chip61} = lists:nth(1, FilteredActions), 
    ?assertEqual(47, ComparisonBot).

part2_integration_test() -> 
    {ok, Binary} = file:read_file("input.txt"),
    RawLines = binary:bin_to_list(Binary),
    SplitLines = string:tokens(RawLines, "\n"),
    ParsedDirections = lists:map(fun factory:parse/1, SplitLines),
    Instructions = lists:filter(fun(X) -> case X of false -> false; _ -> true end end, ParsedDirections),
    PerformedActions = factory:route_instructions_to_bots(Instructions),
    TrimmedActions = lists:map(fun({_, Actions}) -> Actions end, PerformedActions),
    FlattenedActions = lists:flatten(TrimmedActions),
    FilteredActions = lists:filter(fun({Destination, Id, _}) -> (Destination == output) and ((Id == 0) or (Id == 1) or (Id == 2)) end, FlattenedActions),
    OutputValues = lists:foldl(fun({_,_,X}, Prod) -> X * Prod end, 1, FilteredActions),
    io:format("~p~n", [FilteredActions]),
    ?assertEqual(2666, OutputValues).
