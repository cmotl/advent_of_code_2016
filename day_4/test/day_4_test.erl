-module(day_4_test).
-include_lib("eunit/include/eunit.hrl").

part1_integration_test() -> 
    {ok, Binary} = file:read_file("input.txt"),
    RawRooms = binary:bin_to_list(Binary),
    SplitRooms = string:tokens(RawRooms, "\n"),
    ParsedRooms = lists:map(fun(Room) -> room_parser:parse(Room) end, SplitRooms),
    ValidRooms = lists:filter(fun({RoomName,_,Checksum}) -> room_checksum:generate(RoomName) == Checksum end, ParsedRooms),
    SectorIdCount = lists:foldl(fun({_,X,_}, Sum) -> X + Sum end, 0, ValidRooms),
    ?assertEqual(0, SectorIdCount).
