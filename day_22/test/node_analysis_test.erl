-module(node_analysis_test).
-include_lib("eunit/include/eunit.hrl").
-include("src/node.hrl").

-import(node_analysis, [parse/1, viable_node_pairs/1]).

should_parse_node_test() ->
    Node = "/dev/grid/node-x0-y0     85T   68T    17T   80%",
    ExpectedNode = #node{x=0, y=0, size=85, used=68, available=17},
    ?assertEqual(ExpectedNode, parse(Node)).

should_pair_two_viable_nodes_test() ->
    Node1 = #node{used=1, available=1},
    Node2 = #node{used=2, available=2},
    ViableNodes = viable_node_pairs([Node1, Node2]),
    ?assertEqual(1, length(ViableNodes)).

should_exclude_empty_nodes_from_viable_nodes_test() ->
    Node1 = #node{used=2, available=1},
    Node2 = #node{used=2, available=2},
    Node3 = #node{used=0, available=1},
    ViableNodes = viable_node_pairs([Node1, Node2, Node3]),
    ?assertEqual(1, length(ViableNodes)).
