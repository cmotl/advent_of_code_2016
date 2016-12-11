-module(intermediate_locations_test).
-include_lib("eunit/include/eunit.hrl").

-import(intermediate_locations, [intermediate_locations/2]).

should_return_no_locations_when_source_is_destination_test() -> 
    Source = {0,0},
    Destination = {0,0},
    ?assertEqual([], intermediate_locations(Source, Destination)).

should_return_one_locations_when_move_is_one_spot_away_horizontally_test() -> 
    Source = {0,0},
    Destination = {1,0},
    ?assertEqual([{1,0}], intermediate_locations(Source, Destination)).

should_return_one_locations_when_move_is_one_spot_away_negative_horizontally_test() -> 
    Source = {0,0},
    Destination = {-1,0},
    ?assertEqual([{-1,0}], intermediate_locations(Source, Destination)).

should_return_multiple_locations_when_move_is_multiple_spots_away_horizontally_test() -> 
    Source = {0,0},
    Destination = {3,0},
    ?assertEqual([{1,0}, {2,0}, {3,0}], intermediate_locations(Source, Destination)).

should_return_multiple_locations_when_move_is_multiple_spots_away_negative_horizontally_test() -> 
    Source = {0,0},
    Destination = {-3,0},
    ?assertEqual([{-1,0}, {-2,0}, {-3,0}], intermediate_locations(Source, Destination)).

should_return_one_locations_when_move_is_one_spot_away_vertically_test() -> 
    Source = {0,0},
    Destination = {0,1},
    ?assertEqual([{0,1}], intermediate_locations(Source, Destination)).

should_return_one_locations_when_move_is_one_spot_away_negative_vertically_test() -> 
    Source = {0,0},
    Destination = {0,-1},
    ?assertEqual([{0,-1}], intermediate_locations(Source, Destination)).

should_return_multiple_locations_when_move_is_multiple_spots_away_vertically_test() -> 
    Source = {0,0},
    Destination = {0,3},
    ?assertEqual([{0,1}, {0,2}, {0,3}], intermediate_locations(Source, Destination)).

should_return_multiple_locations_when_move_is_multiple_spots_away_negative_vertically_test() -> 
    Source = {0,0},
    Destination = {0,-3},
    ?assertEqual([{0,-1}, {0,-2}, {0,-3}], intermediate_locations(Source, Destination)).

