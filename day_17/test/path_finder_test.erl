-module(path_finder_test).
-include_lib("eunit/include/eunit.hrl").

-import(path_finder, [next_search_space/2, next_search_spaces/4, paths_to_destination/3, longest_path_to_destination/3]).

should_add_open_spaces_to_search_space_from_initial_space_test() ->
    Passcode = "hijkl",
    InputList = [{start,{1,1}}],
    ExpectedSearchSpace = [
        [{down, {1,2}},{start,{1,1}}]
    ],
    ActualSearchSpace = next_search_space(InputList, Passcode),
    ?assertEqual(ExpectedSearchSpace, ActualSearchSpace). 

should_add_open_spaces_to_search_space_from_second_space_test() ->
    Passcode = "hijkl",
    InitialSearchSpace = [{down, {1,2}},{start,{1,1}}],
    ExpectedSearchSpace = [
        [ {up, {1,1}},{down, {1,2}},{start,{1,1}} ],
        [ {right, {2,2}},{down, {1,2}},{start,{1,1}} ]
    ],
    ActualSearchSpace = next_search_space(InitialSearchSpace, Passcode),
    ?assertEqual(ExpectedSearchSpace, ActualSearchSpace). 


should_prune_dead_end_from_live_search_spaces_test() ->
    Passcode = "hijkl",
    InputActiveSearchSpaces = [
        [ {up, {1,1}},{down, {1,2}},{start,{1,1}} ],
        [ {right, {2,2}},{down, {1,2}},{start,{1,1}} ]
	],
    InputDeadEndSearchSpaces = [],
    ExpectedActiveSearchSpaces = [ [ {right, {2,1}}, {up, {1,1}},{down, {1,2}},{start,{1,1}} ] ],
    ExpectedDeadEndSearchSpaces = [ [ {right, {2,2}},{down, {1,2}},{start,{1,1}} ] ],
    {ActualActiveSearchSpaces, ActualDeadEndSearchSpaces} = next_search_spaces(InputActiveSearchSpaces, InputDeadEndSearchSpaces,[], Passcode),
    ?assertEqual( ExpectedDeadEndSearchSpaces, ActualDeadEndSearchSpaces),
    ?assertEqual( ExpectedActiveSearchSpaces, ActualActiveSearchSpaces).


should_branch_into_multiple_search_spaces_test() ->
    Passcode = "hijkl",
    InputActiveSearchSpaces = [
        [ {down, {1,2}},{start,{1,1}} ]
	],
    InputDeadEndSearchSpaces = [],
    ExpectedActiveSearchSpaces = [ 
        [ {up, {1,1}},{down, {1,2}},{start,{1,1}} ] ,
        [ {right, {2,2}},{down, {1,2}},{start,{1,1}} ] 
    ],
    ExpectedDeadEndSearchSpaces = [ ],
    {ActualActiveSearchSpaces, ActualDeadEndSearchSpaces} = next_search_spaces(InputActiveSearchSpaces, InputDeadEndSearchSpaces,[], Passcode),
    ?assertEqual( ExpectedDeadEndSearchSpaces, ActualDeadEndSearchSpaces),
    ?assertEqual( ExpectedActiveSearchSpaces, ActualActiveSearchSpaces).

should_find_paths_to_destination_test() ->
    Passcode = "ihgpwlah",
    StartingLocation = {1,1},
    Destination = {4,4},
    ExpectedActiveSearchSpaces = [[
        {down, {4,4}},
        {right, {4,3}},
        {right, {3,3}},
        {right, {2,3}},
        {down, {1,3}},
        {down, {1,2}},
        {start, {1,1}}
    ]],
    ExpectedDeadEndSearchSpacesCount = 4,
    {ActualActiveSearchSpaces, ActualDeadEndSearchSpaces} = paths_to_destination(StartingLocation, Destination, Passcode),
    ?assertEqual( ExpectedActiveSearchSpaces, ActualActiveSearchSpaces),
    ?assertEqual( ExpectedDeadEndSearchSpacesCount, length(ActualDeadEndSearchSpaces)).

should_find_longest_possible_path_to_destination_test() ->
    Passcode = "ihgpwlah",
    StartingLocation = {1,1},
    Destination = {4,4},
    ActualLongestSearchSpace = longest_path_to_destination(StartingLocation, Destination, Passcode),
    ?assertEqual(370, length(ActualLongestSearchSpace)-1).
