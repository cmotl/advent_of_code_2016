-module(possible_triangle_test).
-include_lib("eunit/include/eunit.hrl").

-import(possible_triangle, [count/1]).

should_return_zero_when_there_are_no_triangles_to_try_test() ->
    PossibleTriangles = [],
    ?assertEqual(0, count(PossibleTriangles)).

should_return_proper_count_when_there_is_one_possible_triangle_to_try_test() ->
    PossibleTriangles = [{3,4,5}],
    ?assertEqual(1, count(PossibleTriangles)).

should_return_proper_count_when_there_is_one_impossible_triangle_to_try_test() ->
    PossibleTriangles = [{3,4,8}],
    ?assertEqual(0, count(PossibleTriangles)).

should_return_proper_count_when_there_are_more_than_one_possible_triangles_to_try_test() ->
    PossibleTriangles = [{3,4,5}, {5,12,13}],
    ?assertEqual(2, count(PossibleTriangles)).

should_return_proper_count_when_there_is_a_possible_triangle_following_an_impossible_triangle_to_try_test() ->
    PossibleTriangles = [{3,4,8}, {5,12,13}],
    ?assertEqual(1, count(PossibleTriangles)).

