-module(tile_test).
-include_lib("eunit/include/eunit.hrl").

-import(tile, [parse/1, generate/3, generate_row/1, generate_rows/2]).

should_parse_a_safe_tile_test() ->
    SafeTile = $.,
    ?assertEqual(safe, parse(SafeTile)).

should_parse_a_trap_tile_test() ->
    TrapTile = $^,
    ?assertEqual(trap, parse(TrapTile)).

should_parse_row_of_tiles_test() ->
    Tiles = "..^^.",
    ?assertEqual([safe, safe, trap, trap, safe], parse(Tiles)).

should_generate_a_trap_tile_when_left_and_center_tiles_are_traps_but_right_is_not_test() ->
    LeftTile = trap,
    CenterTile = trap,
    RightTile = safe,
    ?assertEqual(trap, generate(LeftTile,CenterTile,RightTile)).

should_generate_a_trap_tile_when_center_and_right_tiles_are_traps_but_left_is_not_test() ->
    LeftTile = safe,
    CenterTile = trap,
    RightTile = trap,
    ?assertEqual(trap, generate(LeftTile,CenterTile,RightTile)).

should_generate_a_trap_tile_when_only_left_tile_is_trap_test() ->
    LeftTile = trap,
    CenterTile = safe,
    RightTile = safe,
    ?assertEqual(trap, generate(LeftTile,CenterTile,RightTile)).

should_generate_a_trap_tile_when_only_right_tile_is_trap_test() ->
    LeftTile = safe,
    CenterTile = safe,
    RightTile = trap,
    ?assertEqual(trap, generate(LeftTile,CenterTile,RightTile)).

should_generate_row_of_tiles_test() ->
    Tiles = [safe, safe, trap, trap, safe],
    ExpectedTiles = [safe, trap, trap, trap, trap],
    ?assertEqual(ExpectedTiles, generate_row(Tiles)).

should_generate_requested_number_of_rows_test() ->
    FirstRowTiles = [safe, safe, trap, trap, safe],
    ExpectedRows = [
        [safe, safe, trap, trap, safe],
        [safe, trap, trap, trap, trap],
        [trap, trap, safe, safe, trap]
    ],
    ?assertEqual(ExpectedRows, generate_rows(FirstRowTiles, 3)).
