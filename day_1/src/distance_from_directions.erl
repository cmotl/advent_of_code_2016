-module(distance_from_directions).
-export([distance_from_directions/1]).

distance_from_directions(Directions) ->
    ParsedDirections = lists:map(fun parse_directions:parse_directions/1, Directions),
    FinalDestination = lists:foldl(fun turn_and_move:turn_and_move/2, {north, {0,0}}, ParsedDirections),
    {_,FinalCoordinates} = FinalDestination,
    distance:distance(FinalCoordinates).

