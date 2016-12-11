-module(day_3_test).
-include_lib("eunit/include/eunit.hrl").

part1_integration_test() -> 
    {ok, Binary} = file:read_file("input.txt"),
    RawTriangles = binary:bin_to_list(Binary),
    SplitTriangles = string:tokens(RawTriangles, "\n"),
    PossibleTrianglesSet = lists:map(fun(Triangle) -> 
                Sides = string:tokens(Triangle, " "),
                [S1, S2, S3 |_] = lists:map(fun(Side) -> list_to_integer(Side) end, Sides),
                {S1, S2, S3}
            end, SplitTriangles),
    ?assertEqual(869, possible_triangle:count(PossibleTrianglesSet)).
