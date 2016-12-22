-module(room_parser_test).
-include_lib("eunit/include/eunit.hrl").

-import(room_parser, [parse/1]).

should_parse_room_name_test() ->
    Room = "abcde-3[abcde]",
    {RoomName, _, _} = parse(Room),
    ?assertEqual("abcde", RoomName).

should_parse_room_name_with_multiple_dashes_test() ->
    Room = "a-b-c-d-e-3[abcde]",
    {RoomName, _, _} = parse(Room),
    ?assertEqual("a-b-c-d-e", RoomName).

should_parse_sector_id_test() ->
    Room = "abcde-3[abcde]",
    {_, SectorId, _} = parse(Room),
    ?assertEqual(3, SectorId).

should_parse_multiple_digit_sector_id_test() ->
    Room = "abcde-321[abcde]",
    {_, SectorId, _} = parse(Room),
    ?assertEqual(321, SectorId).

should_parse_checksum_test() ->
    Room = "abcde-321[xyzxy]",
    {_, _, Checksum} = parse(Room),
    ?assertEqual("xyzxy", Checksum).
