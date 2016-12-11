-module(room_parser).
-export([parse/1]).

parseRoomName(Room) ->
    DashIndex = string:rchr(Room, $-),
    string:substr(Room, 1, DashIndex-1).

parseSectorId(Room) -> 
    DashIndex = string:rchr(Room, $-),
    OpenBracketIndex = string:chr(Room, $[),
    SectorIdString = string:substr(Room, DashIndex+1, OpenBracketIndex - (DashIndex + 1)),
    {SectorId, _} = string:to_integer(SectorIdString),
    SectorId.

parse_checksum(Room) ->
    OpenBracketIndex = string:chr(Room, $[),
    CloseBracketIndex = string:chr(Room, $]),
    string:substr(Room, OpenBracketIndex+1, CloseBracketIndex - (OpenBracketIndex + 1)).

parse(Room) -> 
    {parseRoomName(Room), parseSectorId(Room), parse_checksum(Room)}.
