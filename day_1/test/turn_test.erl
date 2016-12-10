-module(turn_test).
-include_lib("eunit/include/eunit.hrl").

should_face_west_when_turning_left_from_north_test() ->
    ?assertEqual(west, turn:turn(north, left)).

should_face_east_when_turning_right_from_north_test() ->
    ?assertEqual(east, turn:turn(north, right)).

should_face_south_when_turning_left_from_west_test() ->
    ?assertEqual(south, turn:turn(west, left)).

should_face_north_when_turning_right_from_west_test() ->
    ?assertEqual(north, turn:turn(west, right)).

should_face_east_when_turning_left_from_south_test() ->
    ?assertEqual(east, turn:turn(south, left)).

should_face_west_when_turning_right_from_south_test() ->
    ?assertEqual(west, turn:turn(south, right)).

should_face_north_when_turning_left_from_east_test() ->
    ?assertEqual(north, turn:turn(east, left)).

should_face_south_when_turning_right_from_east_test() ->
    ?assertEqual(south, turn:turn(east, right)).

