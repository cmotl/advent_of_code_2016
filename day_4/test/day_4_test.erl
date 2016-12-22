-module(day_4_test).
-include_lib("eunit/include/eunit.hrl").

part1_integration_test() -> 
    {ok, Binary} = file:read_file("input.txt"),
    RawRooms = binary:bin_to_list(Binary),
    SplitRooms = string:tokens(RawRooms, "\n"),
    ParsedRooms = lists:map(fun(Room) -> room_parser:parse(Room) end, SplitRooms),
    ValidRooms = lists:filter(fun({RoomName,_,Checksum}) -> room_checksum:generate(RoomName) == Checksum end, ParsedRooms),
    SectorIdCount = lists:foldl(fun({_,X,_}, Sum) -> X + Sum end, 0, ValidRooms),
    ?assertEqual(137896, SectorIdCount).

part2_integration_test() ->
    {ok, Binary} = file:read_file("input.txt"),
    RawRooms = binary:bin_to_list(Binary),
    SplitRooms = string:tokens(RawRooms, "\n"),
    ParsedRooms = lists:map(fun(Room) -> room_parser:parse(Room) end, SplitRooms),
    ValidRooms = lists:filter(fun({RoomName,_,Checksum}) -> room_checksum:generate(RoomName) == Checksum end, ParsedRooms),
    DecyptedRooms = lists:map(fun({RoomName, SectorId, _}) -> {shift_cipher:decrypt(RoomName, SectorId),SectorId} end, ValidRooms),
    NorthPoleRoom = lists:filter(fun({RoomName, _}) -> string:str(RoomName, "northpole") =/= 0 end, DecyptedRooms),
    [{NorthPoleRoomName,NorthPoleRoomSectorId}|_] = NorthPoleRoom,
    ?assertEqual("northpole object storage", NorthPoleRoomName),
    ?assertEqual(501, NorthPoleRoomSectorId).
