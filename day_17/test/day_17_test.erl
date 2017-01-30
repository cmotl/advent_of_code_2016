-module(day_17_test).
-include_lib("eunit/include/eunit.hrl").

part1_integration_test() -> 
    Passcode = "vwbaicqe",
    StartingLocation = {1,1},
    Destination = {4,4},
    {ActiveSearchSpaces, _} = path_finder:paths_to_destination(StartingLocation, Destination, Passcode),
    SearchSpaceDirections = lists:map(fun room:direction_to_path_code/1, hd(ActiveSearchSpaces)),
    FilteredSearchSpaceDirections = lists:filter(fun(X) -> X =/= $S end, SearchSpaceDirections),
    ?assertEqual("DRDRULRDRD", lists:reverse(FilteredSearchSpaceDirections)).

part2_integration_test() -> 
    Passcode = "vwbaicqe",
    StartingLocation = {1,1},
    Destination = {4,4},
    ActualLongestSearchSpace = path_finder:longest_path_to_destination(StartingLocation, Destination, Passcode),
    ?assertEqual(0, length(ActualLongestSearchSpace)-1).
