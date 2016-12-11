-module(triangle_test).
-include_lib("eunit/include/eunit.hrl").

-import(triangle, [is_valid_triangle/3]).

should_be_valid_when_side1_and_side2_are_greater_than_side3_test() -> 
    ?assertEqual(true, is_valid_triangle(3,4,5)).

should_not_be_valid_when_side3_is_greater_than_side1_and_side2_test() -> 
    ?assertEqual(false, is_valid_triangle(3,4,8)).


should_be_valid_when_side1_and_side3_are_greater_than_side2_test() -> 
    ?assertEqual(true, is_valid_triangle(5,12,13)).

should_not_be_valid_when_side2_is_greater_than_side1_and_side3_test() -> 
    ?assertEqual(false, is_valid_triangle(3,12,8)).


should_be_valid_when_side2_and_side3_are_greater_than_side1_test() -> 
    ?assertEqual(true, is_valid_triangle(8,15,17)).

should_not_be_valid_when_side1_is_greater_than_side2_and_side3_test() -> 
    ?assertEqual(false, is_valid_triangle(8,4,3)).

