-module(instruction).
-export([parse/1]).

parse(X) when is_list(X) -> 
    Instructions = lists:foldl(fun(Instruction, Instructions) -> [parse(Instruction)|Instructions] end, [], X),
    lists:reverse(Instructions);

parse(X) when X == $U -> up;
parse(X) when X == $D -> down;
parse(X) when X == $L -> left;
parse(X) when X == $R -> right.
