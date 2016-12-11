-module(move_to_second_visited_location).
-export([move_to_second_visited_location/2, move_to_second_visited_location/3]).

move_to_second_visited_location(CurrentLocation, Directions) -> 
    {_,OriginCoordinates} = CurrentLocation,
    move_to_second_visited_location(CurrentLocation, Directions, [OriginCoordinates]).

move_to_second_visited_location(_,[],_) -> nil;
move_to_second_visited_location(CurrentLocation,[DirectionToMove|Rest],PreviousLocations) ->
    NewLocation = turn_and_move:turn_and_move(DirectionToMove, CurrentLocation),
    {_, Coordinates} = CurrentLocation,
    {_,NewCoordinates} = NewLocation,
    LocationsToVisit = intermediate_locations:intermediate_locations(Coordinates, NewCoordinates),
    SecondVisitedLocation = final_destination:final_destination(LocationsToVisit, PreviousLocations),
    case SecondVisitedLocation of
        nil -> move_to_second_visited_location(NewLocation, Rest, PreviousLocations ++ LocationsToVisit);
        _ -> SecondVisitedLocation
    end.
