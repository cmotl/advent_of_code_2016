-module(node_analysis).
-export([parse/1, viable_node_pairs/1]).

-include("node.hrl").

parse_node_coordinates(NodeName) ->
    [_, [$x|X], [$y|Y]] = re:split(NodeName, "-", [{return, list}]),
    {list_to_integer(X), list_to_integer(Y)}.

parse_size(Size) ->
    [$T|Rest] = lists:reverse(Size),
    list_to_integer(lists:reverse(Rest)).

parse(RawNode) -> 
    [Coordinates, Size, Used, Available, _] = re:split(RawNode, "\\s+", [{return, list}]),
    {X, Y} = parse_node_coordinates(Coordinates),
    NodeSize = parse_size(Size),
    NodeUsed = parse_size(Used),
    NodeAvailable = parse_size(Available),
    #node{x=X, y=Y, size=NodeSize, used=NodeUsed, available=NodeAvailable}.

viable_node_pairs(Nodes) ->
    [{A,B} || A <- Nodes, B <- Nodes, A =/= B, A#node.used =/= 0, A#node.used =< B#node.available].

