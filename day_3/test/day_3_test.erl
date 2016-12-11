-module(day_3_test).
-include_lib("eunit/include/eunit.hrl").

regroup_records([]) -> [];
regroup_records([[A1,A2,A3],[B1,B2,B3],[C1,C2,C3]|Rest]) -> 
    [[A1,B1,C1],[A2,B2,C2],[A3,B3,C3] | regroup_records(Rest)].

should_regroup_if_no_records_test() ->
    Records = [],
    ?assertEqual([], regroup_records(Records)).

should_regroup_single_set_three_records_test() ->
    Records = [[1,2,3],[1,2,3],[1,2,3]],
    ?assertEqual([[1,1,1],[2,2,2],[3,3,3]], regroup_records(Records)).

should_regroup_multiple_sets_of_three_records_test() ->
    Records = [[1,2,3],[1,2,3],[1,2,3],[4,5,6],[4,5,6],[4,5,6]],
    ?assertEqual([[1,1,1],[2,2,2],[3,3,3],[4,4,4],[5,5,5],[6,6,6]], regroup_records(Records)).


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

part2_integration_test() -> 
    {ok, Binary} = file:read_file("input.txt"),
    RawTriangles = binary:bin_to_list(Binary),
    SplitTriangles = string:tokens(RawTriangles, "\n"),
    SplitSides = lists:map(fun(Group) -> string:tokens(Group, " ") end, SplitTriangles),
    CorrectedSides = regroup_records(SplitSides),
    PossibleTrianglesSet = lists:map(fun(Sides) -> 
                [S1, S2, S3 |_] = lists:map(fun(Side) -> list_to_integer(Side) end, Sides),
                {S1, S2, S3}
            end, CorrectedSides),
    ?assertEqual(869, possible_triangle:count(PossibleTrianglesSet)).
