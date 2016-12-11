-module(final_destination).
-export([final_destination/2]).

final_destination(_, []) -> nil;
final_destination([], _) -> nil;
final_destination([Location|Rest], PreviousVisitiedLocations) ->
    case lists:member(Location, PreviousVisitiedLocations) of
        true -> Location;
        false -> final_destination(Rest, PreviousVisitiedLocations)
    end.
