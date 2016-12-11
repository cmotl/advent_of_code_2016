-module(final_destination_test).
-include_lib("eunit/include/eunit.hrl").

-import(final_destination, [final_destination/2]).

should_return_nil_if_no_previous_locations_test() ->
    LocationsToVisit = [{0,0}],
    PreviousVisitedLocations = [],
    ?assertEqual(nil, final_destination(LocationsToVisit, PreviousVisitedLocations)).

should_return_nil_if_no_Locations_to_visit_test() ->
    LocationsToVisit = [],
    PreviousVisitedLocations = [{0,0}],
    ?assertEqual(nil, final_destination(LocationsToVisit, PreviousVisitedLocations)).

should_find_single_location_to_visit_when_it_has_been_previously_visited_test() ->
    LocationsToVisit = [{0,0}],
    PreviousVisitedLocations = [{0,0}],
    ?assertEqual({0,0}, final_destination(LocationsToVisit, PreviousVisitedLocations)).

should_find_location_in_multiple_locations_when_it_has_been_previously_visited_test() ->
    LocationsToVisit = [{1,1},{0,0}],
    PreviousVisitedLocations = [{0,0}],
    ?assertEqual({0,0}, final_destination(LocationsToVisit, PreviousVisitedLocations)).

should_find_location_in_multiple_locations_when_multiple_locations_have_been_previously_visited_test() ->
    LocationsToVisit = [{1,1},{0,0}],
    PreviousVisitedLocations = [{2,2},{0,0},{3,3}],
    ?assertEqual({0,0}, final_destination(LocationsToVisit, PreviousVisitedLocations)).

should_return_nil_when_none_of_multiple_locations_have_been_previously_visited_test() ->
    LocationsToVisit = [{0,0},{1,1}],
    PreviousVisitedLocations = [{2,2},{3,3}],
    ?assertEqual(nil, final_destination(LocationsToVisit, PreviousVisitedLocations)).
