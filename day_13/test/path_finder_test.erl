-module(path_finder_test).
-include_lib("eunit/include/eunit.hrl").

-import(path_finder,[surrounding_locations/1, next_search_space/2, next_search_spaces/4, paths_to_destination/3, unique_locations_in_steps/3]).

should_return_surrounding_locations_test() ->
    ?assertEqual([{1,0},{0,1},{1,2},{2,1}], surrounding_locations({1,1})).

should_return_surrounding_locations_without_crossing_X_or_Y_boundary_test() ->
    ?assertEqual([{0,1}, {1,0}], surrounding_locations({0,0})).


should_add_open_spaces_to_search_space_from_initial_space_test() ->
    {ok, Schematic} = schematic:start(10),
    InputList = [{1,1}],
    ExpectedSearchSpace = [
        [{0,1},{1,1}],
        [{1,2},{1,1}]
    ],
    ActualSearchSpace = next_search_space(InputList, Schematic),
    ?assertEqual(ExpectedSearchSpace, ActualSearchSpace). 

should_add_open_spaces_to_search_space_from_second_space_test() ->
    {ok, Schematic} = schematic:start(10),
    InitialSearchSpace = [{1,2},{1,1}],
    ExpectedSearchSpace = [
        [{2,2},{1,2},{1,1}]
    ],
    ActualSearchSpace = next_search_space(InitialSearchSpace, Schematic),
    ?assertEqual(ExpectedSearchSpace, ActualSearchSpace). 
    

should_prune_dead_end_from_live_search_spaces_test() ->
    {ok, Schematic} = schematic:start(10),
    InputActiveSearchSpaces = [
       [{0,0},{0,1},{1,1}] 
    ],
    InputDeadEndSearchSpaces = [],
    ExpectedActiveSearchSpaces = [],
    ExpectedDeadEndSearchSpaces = [[{0,0},{0,1},{1,1}]],
    {ActualActiveSearchSpaces, ActualDeadEndSearchSpaces} = next_search_spaces(InputActiveSearchSpaces, InputDeadEndSearchSpaces,[], Schematic),
    ?assertEqual( ExpectedActiveSearchSpaces, ActualActiveSearchSpaces),
    ?assertEqual( ExpectedDeadEndSearchSpaces, ActualDeadEndSearchSpaces).

should_continue_active_search_from_live_search_spaces_test() ->
    {ok, Schematic} = schematic:start(10),
    InputActiveSearchSpaces = [
       [{0,1},{1,1}] 
    ],
    InputDeadEndSearchSpaces = [],
    ExpectedActiveSearchSpaces = [[{0,0},{0,1},{1,1}]],
    ExpectedDeadEndSearchSpaces = [],
    {ActualActiveSearchSpaces, ActualDeadEndSearchSpaces} = next_search_spaces(InputActiveSearchSpaces, InputDeadEndSearchSpaces,[], Schematic),
    ?assertEqual( ExpectedActiveSearchSpaces, ActualActiveSearchSpaces),
    ?assertEqual( ExpectedDeadEndSearchSpaces, ActualDeadEndSearchSpaces).

should_branch_into_multiple_search_spaces_test() ->
    {ok, Schematic} = schematic:start(10),
    InputActiveSearchSpaces = [
       [{1,1}] 
    ],
    InputDeadEndSearchSpaces = [],
    ExpectedActiveSearchSpaces = [[{0,1},{1,1}], [{1,2},{1,1}]],
    ExpectedDeadEndSearchSpaces = [],
    {ActualActiveSearchSpaces, ActualDeadEndSearchSpaces} = next_search_spaces(InputActiveSearchSpaces, InputDeadEndSearchSpaces,[], Schematic),
    ?assertEqual( ExpectedActiveSearchSpaces, ActualActiveSearchSpaces),
    ?assertEqual( ExpectedDeadEndSearchSpaces, ActualDeadEndSearchSpaces).


should_find_paths_to_destination_test() ->
    {ok, Schematic} = schematic:start(10),
    StartingLocation = {1,1},
    Destination = {0,0},
    ExpectedActiveSearchSpaces = [[{0,0},{0,1},{1,1}]],
    ExpectedDeadEndSearchSpaces = [],
    {ActualActiveSearchSpaces, ActualDeadEndSearchSpaces} = paths_to_destination(StartingLocation, Destination, Schematic),
    ?assertEqual( ExpectedActiveSearchSpaces, ActualActiveSearchSpaces),
    ?assertEqual( ExpectedDeadEndSearchSpaces, ActualDeadEndSearchSpaces).

should_find_paths_to_destination_with_deadends_test() ->
    {ok, Schematic} = schematic:start(10),
    StartingLocation = {1,1},
    Destination = {3,2},
    ExpectedActiveSearchSpaces = [[{3,2},{2,2},{1,2},{1,1}]],
    ExpectedDeadEndSearchSpaces = [[{0,0},{0,1},{1,1}]],
    {ActualActiveSearchSpaces, ActualDeadEndSearchSpaces} = paths_to_destination(StartingLocation, Destination, Schematic),
    ?assertEqual( ExpectedActiveSearchSpaces, ActualActiveSearchSpaces),
    ?assertEqual( ExpectedDeadEndSearchSpaces, ActualDeadEndSearchSpaces).

should_count_unique_locations_when_distance_is_one_test() ->
    {ok, Schematic} = schematic:start(10),
    StartingLocation = {1,1},
    NumberOfSteps = 1,
    ExpectedUniqueLocations = [{1,1},{0,1},{1,2}],
    ActualUniqueLocations = unique_locations_in_steps(StartingLocation, NumberOfSteps, Schematic),
    ?assertEqual( ExpectedUniqueLocations, ActualUniqueLocations).

should_count_unique_locations_when_distance_is_two_test() ->
    {ok, Schematic} = schematic:start(10),
    StartingLocation = {1,1},
    NumberOfSteps = 2,
    ExpectedUniqueLocations = [{0,0},{1,1},{2,2},{0,1},{1,2}],
    ActualUniqueLocations = unique_locations_in_steps(StartingLocation, NumberOfSteps, Schematic),
    ?assertEqual( ExpectedUniqueLocations, ActualUniqueLocations).
