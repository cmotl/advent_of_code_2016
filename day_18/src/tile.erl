-module(tile).
-export([parse/1, generate/3, generate_row/1, generate_rows/2, generate_safe_tile_count_in_number_of_rows/2]).

parse($.) -> safe;
parse($^) -> trap;
parse(Tiles) when is_list(Tiles) -> lists:map(fun parse/1, Tiles).

generate(trap, trap, safe) -> trap;
generate(safe, trap, trap) -> trap;
generate(trap, safe, safe) -> trap;
generate(safe, safe, trap) -> trap;
generate(_Left,_Center,_Right) -> safe.

generate_row([_,_|[]], NextRow) -> lists:reverse(NextRow);
generate_row(Tiles, NextRow) -> 
    [Left,Center,Right | Rest] = Tiles,
    NextTile = generate(Left,Center,Right),
    generate_row([Center,Right|Rest], [NextTile|NextRow]).
generate_row(Tiles) -> 
    generate_row(lists:append([ [safe],Tiles,[safe] ]), []).

generate_rows(_PreviousTiles, GeneratedRows, 0) -> lists:reverse(GeneratedRows);
generate_rows(PreviousTiles, GeneratedRows, NumberOfRows) -> 
    NextRow = generate_row(PreviousTiles),
    generate_rows(NextRow, [NextRow|GeneratedRows], NumberOfRows-1).
generate_rows(FirstRowTiles, NumberOfRows) -> 
    generate_rows(FirstRowTiles, [FirstRowTiles], NumberOfRows-1).

filter_safe_tiles(Tiles) -> lists:filter(fun(X) -> X == safe end, Tiles).

generate_safe_tile_count_in_number_of_rows(_PreviousTiles, SafeCount, 0) -> SafeCount;
generate_safe_tile_count_in_number_of_rows(PreviousTiles, SafeCount, NumberOfRows) ->
    NextRow = generate_row(PreviousTiles),
    generate_safe_tile_count_in_number_of_rows(NextRow, SafeCount + length(filter_safe_tiles(NextRow)), NumberOfRows-1).

generate_safe_tile_count_in_number_of_rows(FirstRowTiles, NumberOfRows) ->
    generate_safe_tile_count_in_number_of_rows(FirstRowTiles, length(filter_safe_tiles(FirstRowTiles)), NumberOfRows-1).
    
