-module(day_1_test).
-include_lib("eunit/include/eunit.hrl").

-import(distance_from_directions, [distance_from_directions/1]).
-import(distance_of_second_visited_location_from_directions, [distance_of_second_visited_location_from_directions/1]).

part1_integration1_test() -> 
    Directions = ["R2", "L3"],
    ?assertEqual(5, distance_from_directions(Directions)).

part1_integration2_test() -> 
    Directions = ["R2", "R2", "R2"],
    ?assertEqual(2, distance_from_directions(Directions)).

part1_integration3_test() -> 
    Directions = ["R5", "L5", "R5", "R3"],
    ?assertEqual(12, distance_from_directions(Directions)).

part1_integration4_test() -> 
    {ok, Binary} = file:read_file("input.txt"),
    Directions = binary:bin_to_list(Binary),
    GoodDirections = string:tokens(Directions, ", \n"),
    ?assertEqual(161, distance_from_directions(GoodDirections)).

part2_integration1_test() ->
    Directions = ["R8", "R4", "R4", "R8"],
    ?assertEqual(4, distance_of_second_visited_location_from_directions(Directions)).

part2_integration2_test() -> 
    {ok, Binary} = file:read_file("input.txt"),
    Directions = binary:bin_to_list(Binary),
    GoodDirections = string:tokens(Directions, ", \n"),
    ?assertEqual(110, distance_of_second_visited_location_from_directions(GoodDirections)).
