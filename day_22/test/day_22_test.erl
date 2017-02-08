-module(day_22_test).
-include_lib("eunit/include/eunit.hrl").

part1_integration_test() -> 
	{ok, Binary} = file:read_file("input.txt"),
    RawLines = binary:bin_to_list(Binary),
    SplitLines = string:tokens(RawLines, "\n"),
    NodeDescriptions = lists:map(fun node_analysis:parse/1, lists:nthtail(2, SplitLines)),
    ViableNodePairs = node_analysis:viable_node_pairs(NodeDescriptions),
    ?assertEqual(967, length(ViableNodePairs)).

part2_integration_test() -> 
	{ok, Binary} = file:read_file("input.txt"),
    RawLines = binary:bin_to_list(Binary),
    SplitLines = string:tokens(RawLines, "\n"),
    NodeDescriptions = lists:map(fun node_analysis:parse/1, lists:nthtail(2, SplitLines)),
    ?assertEqual(990, length(NodeDescriptions)).
