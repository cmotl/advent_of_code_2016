-module(possible_triangle).
-export([count/1]).

count([]) -> 0;
count([{S1,S2,S3}|Rest]) -> 
    case triangle:is_valid_triangle(S1,S2,S3) of
        true -> 1 + count(Rest);
        false -> 0 + count(Rest)
    end.
