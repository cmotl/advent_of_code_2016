-module(room_test).
-include_lib("eunit/include/eunit.hrl").

-import(room, [surrounding_rooms/1, unlocked_surrounding_rooms/3]).

should_get_surrounding_rooms_test() ->
    InitialRoom = {2,2},
    ExpectedRooms = [{up, {2,1}},{down, {2,3}},{left, {1,2}},{right, {3,2}}],
    ActualRooms = surrounding_rooms(InitialRoom),
    ?assertEqual(ExpectedRooms, ActualRooms).

should_honor_walls_up_and_left_test() ->
    InitialRoom = {1,1},
    ExpectedRooms = [{down, {1,2}},{right, {2,1}}],
    ActualRooms = surrounding_rooms(InitialRoom),
    ?assertEqual(ExpectedRooms, ActualRooms).

should_honor_walls_down_and_right_test() ->
    InitialRoom = {4,4},
    ExpectedRooms = [{up, {4,3}},{left, {3,4}}],
    ActualRooms = surrounding_rooms(InitialRoom),
    ?assertEqual(ExpectedRooms, ActualRooms).

should_report_unlocked_doors_to_surrounding_rooms_test() ->
    InitialRoom = {1,1},
    Passcode = "hijkl",
    History = [],
    ExpectedRooms = [{down, {1,2}}],
    ActualRooms = unlocked_surrounding_rooms(Passcode, History, InitialRoom),
    ?assertEqual(ExpectedRooms, ActualRooms).

should_report_unlocked_doors_to_surrounding_rooms_with_history_test() ->
    InitialRoom = {1,2},
    Passcode = "hijkl",
    History = [{down,{}}],
    ExpectedRooms = [{up, {1,1}}, {right, {2,2}}],
    ActualRooms = unlocked_surrounding_rooms(Passcode, History, InitialRoom),
    ?assertEqual(ExpectedRooms, ActualRooms).
    
    
