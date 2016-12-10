-module(move_test).
-include_lib("eunit/include/eunit.hrl").

should_decrease_x_coordinate_by_move_amount_when_moving_west_test() ->
    {X_Coordinate,_} = move:move(west, {0,0}, 5),
    ?assertEqual(-5, X_Coordinate).

should_not_change_y_coordinate_when_moving_west_test() ->
    {_,Y_Coordinate} = move:move(west, {0,0}, 5),
    ?assertEqual(0, Y_Coordinate).


should_increase_x_coordinate_by_move_amount_when_moving_east_test() ->
    {X_Coordinate,_} = move:move(east, {0,0}, 5),
    ?assertEqual(5, X_Coordinate).

should_not_change_y_coordinate_when_moving_east_test() ->
    {_,Y_Coordinate} = move:move(east, {0,0}, 5),
    ?assertEqual(0, Y_Coordinate).


should_increase_y_coordinate_by_move_amount_when_moving_north_test() ->
    {_,Y_Coordinate}  = move:move(north, {0,0}, 5),
    ?assertEqual(5, Y_Coordinate).

should_not_change_x_coordinate_when_moving_north_test() ->
    {X_Coordinate,_}  = move:move(north, {0,0}, 5),
    ?assertEqual(0, X_Coordinate).


should_decrease_y_coordinate_by_move_amount_when_moving_south_test() ->
    {_,Y_Coordinate} = move:move(south, {0,0}, 5),
    ?assertEqual(-5, Y_Coordinate).

should_not_change_x_coordinate_when_moving_south_test() ->
    {X_Coordinate,_}  = move:move(south, {0,0}, 5),
    ?assertEqual(0, X_Coordinate).
