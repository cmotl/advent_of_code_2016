-module(triangle).
-export([is_valid_triangle/3]).

is_valid_triangle(Side1, Side2, Side3) when Side1 + Side2 =< Side3 -> false;
is_valid_triangle(Side1, Side2, Side3) when Side1 + Side3 =< Side2 -> false;
is_valid_triangle(Side1, Side2, Side3) when Side2 + Side3 =< Side1 -> false;
is_valid_triangle(_,_,_) -> true.
