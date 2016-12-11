-module(distance_of_second_visited_location_from_directions).
-export([distance_of_second_visited_location_from_directions/1]).

distance_of_second_visited_location_from_directions(Directions) ->
    ParsedDirections = lists:map(fun parse_directions:parse_directions/1, Directions),
    FinalCoordinates = move_to_second_visited_location:move_to_second_visited_location({north, {0,0}}, ParsedDirections),
    distance:distance(FinalCoordinates).
