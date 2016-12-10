-module(parse_directions).
-export([parse_directions/1]).

parse_directions([H|T]) -> {parse_direction(H), parse_direction(T)}.

parse_direction(X) when X == $L -> left;
parse_direction(X) when X == $R -> right;
parse_direction(Blocks) -> list_to_integer(Blocks).
