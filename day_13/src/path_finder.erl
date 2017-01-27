-module(path_finder).
-export([surrounding_locations/1, surrounding_open_locations/2, next_search_space/2, next_search_spaces/4, paths_to_destination/3, unique_locations_in_steps/3]).


north_location({_,Y}) when Y =< 0 -> nil;
north_location({X,Y}) -> {X, Y-1}.

west_location({X,_}) when X =< 0-> nil;
west_location({X,Y}) -> {X-1, Y}.

south_location({X,Y}) -> {X, Y+1}.

east_location({X,Y}) -> {X+1, Y}.

surrounding_locations(Location) ->
    SurroundingLocations = [north_location(Location), west_location(Location), south_location(Location), east_location(Location)],
    lists:filter(fun(X) -> X =/= nil end, SurroundingLocations). 

surrounding_open_locations(Location, Schematic) ->
    SurroundingLocations = surrounding_locations(Location),
    lists:filter(fun({X,Y}) -> schematic:element_at(Schematic,X,Y)  == open_space end, SurroundingLocations).


next_search_space([CurrentLocation | _] = CurrentSearchSpace, Schematic) ->
    SurroundingLocations = surrounding_open_locations(CurrentLocation, Schematic),
    [ [X|CurrentSearchSpace] || X <- SurroundingLocations, not lists:member(X, CurrentSearchSpace)].
%next_search_space([CurrentLocation , PreviousLocation | _] = CurrentSearchSpace) ->
%    SurroundingLocations = surrounding_locations(CurrentLocation),
%    [ [X|CurrentSearchSpace] || X <- SurroundingLocations, X =/= PreviousLocation, not lists:member(X, CurrentSearchSpace)].



next_search_spaces([], DeadEnd, NextActive, _) -> {NextActive, DeadEnd};
next_search_spaces([Active|Rest], DeadEnd, NextActive, Schematic) ->
    NextSearchSpaces = next_search_space(Active, Schematic),
    case length(NextSearchSpaces) of 
        0 -> NewDeadEnd = [Active|DeadEnd],
             next_search_spaces(Rest, NewDeadEnd, NextActive, Schematic);
        _ -> next_search_spaces(Rest, DeadEnd, NextSearchSpaces ++ NextActive, Schematic)
    end.



paths_to_destination(Start, Destination, Schematic) -> 
    paths_to_destination(Destination,[[Start]],[], Schematic). 
paths_to_destination(Destination, ActiveSearchSpace, DeadEndSeachSpaces, Schematic) ->
    {NextSearchSpace, NewDeadEndSearchSpaces} = next_search_spaces(ActiveSearchSpace, DeadEndSeachSpaces, [], Schematic),
    case lists:member(Destination, lists:flatten(NextSearchSpace)) of
        true -> PathsWithDestination = lists:filter(fun(X) -> lists:member(Destination, X) end, NextSearchSpace),
                {PathsWithDestination, NewDeadEndSearchSpaces};
        false -> paths_to_destination(Destination, NextSearchSpace, NewDeadEndSearchSpaces, Schematic)
    end.


unique_locations_in_steps(Start, NumberOfSteps, Schematic) -> 
    unique_locations_in_steps(NumberOfSteps,[[Start]],[], Schematic). 
unique_locations_in_steps(NumberOfSteps, ActiveSearchSpace, DeadEndSeachSpaces, Schematic) ->
    {NextSearchSpace, NewDeadEndSearchSpaces} = next_search_spaces(ActiveSearchSpace, DeadEndSeachSpaces, [], Schematic),
    case lists:any(fun(X) -> length(X) =:= NumberOfSteps end, ActiveSearchSpace) of
        true -> 
                Set = sets:from_list(lists:flatten(NextSearchSpace) ++ lists:flatten(NewDeadEndSearchSpaces)),
                sets:to_list(Set);
        false -> unique_locations_in_steps(NumberOfSteps, NextSearchSpace, NewDeadEndSearchSpaces, Schematic)
    end.

