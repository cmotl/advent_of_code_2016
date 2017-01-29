-module(path_finder).
-export([next_search_space/2, next_search_spaces/4, paths_to_destination/3, longest_path_to_destination/3]).

next_search_space([{_, CurrentLocation} | _] = CurrentSearchSpace, Passcode) ->
    SurroundingLocations = room:unlocked_surrounding_rooms(Passcode, CurrentSearchSpace, CurrentLocation),
    [ [X|CurrentSearchSpace] || X <- SurroundingLocations ].

next_search_spaces([], DeadEnd, NextActive, _) -> {NextActive, DeadEnd};
next_search_spaces([Active|Rest], DeadEnd, NextActive, Passcode) ->
    NextSearchSpaces = next_search_space(Active, Passcode),
    case length(NextSearchSpaces) of 
        0 -> NewDeadEnd = [Active|DeadEnd],
             next_search_spaces(Rest, NewDeadEnd, NextActive, Passcode);
        _ -> next_search_spaces(Rest, DeadEnd, NextSearchSpaces ++ NextActive, Passcode)
    end.

paths_to_destination(Start, Destination, Passcode) -> 
    paths_to_destination(Destination,[[{start,Start}]],[], Passcode). 
paths_to_destination(Destination, ActiveSearchSpace, DeadEndSeachSpaces, Passcode) ->
    {NextSearchSpace, NewDeadEndSearchSpaces} = next_search_spaces(ActiveSearchSpace, DeadEndSeachSpaces, [], Passcode),
    case lists:member(Destination, lists:map(fun({_,X}) -> X end, lists:flatten(NextSearchSpace))) of
        true -> PathsWithDestination = lists:filter(fun(X) -> lists:member(Destination, lists:map(fun({_,Y}) -> Y end, X)) end, NextSearchSpace),
                {PathsWithDestination, NewDeadEndSearchSpaces};
        false -> paths_to_destination(Destination, NextSearchSpace, NewDeadEndSearchSpaces, Passcode)
    end.

remove_completed_searches(SearchSpaces, CompletedSearchSpaces, Destination) ->
    {RecentlyCompletedSearchSpaces, ActiveSearchSpaces} = lists:partition(
        fun(X) -> lists:member(Destination, lists:map(fun({_,Y}) -> Y end, X)) end,
        SearchSpaces
    ),
    {ActiveSearchSpaces, lists:append([CompletedSearchSpaces, RecentlyCompletedSearchSpaces])}.

longest_path_to_destination(Start, Destination, Passcode) -> 
    longest_path_to_destination(Destination,[[{start,Start}]], [], [], Passcode). 
longest_path_to_destination(Destination, ActiveSearchSpace, DeadEndSeachSpaces, CompletedSearchSpaces, Passcode) ->
    {NextSearchSpace, NewDeadEndSearchSpaces} = next_search_spaces(ActiveSearchSpace, DeadEndSeachSpaces, [], Passcode),
    {NextActiveSearchSpace, AllCompletedSearchSpaces} = remove_completed_searches(NextSearchSpace, CompletedSearchSpaces, Destination),
    case length(NextActiveSearchSpace) of
        0 -> hd(lists:sort(fun(A,B) -> length(A) > length(B) end, AllCompletedSearchSpaces));
        _ -> longest_path_to_destination(Destination, NextActiveSearchSpace, NewDeadEndSearchSpaces, AllCompletedSearchSpaces, Passcode)
    end.

