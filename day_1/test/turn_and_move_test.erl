-module(turn_and_move_test).
-include_lib("eunit/include/eunit.hrl").

should_turn_correctly_when_moving_test() ->
    CurrentLocation = {north, {0, 0}},
    DirectionToMove = {left, 10},
    {NewDirection,NewCoordinates} = turn_and_move:turn_and_move(DirectionToMove, CurrentLocation),
    ?assertEqual(west, NewDirection),
    ?assertEqual({-10,0},NewCoordinates).
