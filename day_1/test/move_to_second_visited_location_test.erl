-module(move_to_second_visited_location_test).
-include_lib("eunit/include/eunit.hrl").

-import(move_to_second_visited_location, [move_to_second_visited_location/2, move_to_second_visited_location/3]).

should_find_second_visited_location_in_first_direction_test() ->
    CurrentLocation = {west, {0,-1}},
    Directions = [{right, 1}],
    PreviousVisitedLocations = [{0,0}],
    ?assertEqual({0,0}, move_to_second_visited_location(CurrentLocation, Directions, PreviousVisitedLocations)).

should_not_find_visited_location_in_first_direction_test() ->
    CurrentLocation = {west, {0,-1}},
    Directions = [{right, 1}],
    PreviousVisitedLocations = [{1,1}],
    ?assertEqual(nil, move_to_second_visited_location(CurrentLocation, Directions,PreviousVisitedLocations)).

should_find_second_visited_location_after_first_direction_test() ->
    CurrentLocation = {north, {0,0}},
    Directions = [{right, 1}, {left, 1}],
    PreviousVisitedLocations = [{0,0},{1,1}],
    ?assertEqual({1,1}, move_to_second_visited_location(CurrentLocation, Directions,PreviousVisitedLocations)).

should_find_second_visited_location_after_first_direction_with_negative_location_test() ->
    CurrentLocation = {north, {0,0}},
    Directions = [{right, 1}, {right, 1}],
    PreviousVisitedLocations = [{0,0},{1,-1}],
    ?assertEqual({1,-1}, move_to_second_visited_location(CurrentLocation, Directions,PreviousVisitedLocations)).
    
should_store_previous_locations_to_be_found_including_the_origin_test() ->
    CurrentLocation = {north, {0,0}},
    Directions = [{right, 1}, {right, 1}, {right, 1},{right, 1}],
    ?assertEqual({0,0}, move_to_second_visited_location(CurrentLocation, Directions)).
    
