-module(day_18_test).
-include_lib("eunit/include/eunit.hrl").

part1_integration_test() -> 
    {ok, Binary} = file:read_file("input.txt"),
    RawLines = binary:bin_to_list(Binary),
    SplitLines = string:tokens(RawLines, "\n"),
    FirstSetOfTiles = tile:parse(hd(SplitLines)),
    FullSetOfTiles = tile:generate_rows(FirstSetOfTiles, 40),
    SafeTiles = lists:filter(fun(X) -> X == safe end, lists:flatten(FullSetOfTiles)),
    ?assertEqual(1913, length(SafeTiles)).

part2_integration_test() -> 
    {ok, Binary} = file:read_file("input.txt"),
    RawLines = binary:bin_to_list(Binary),
    SplitLines = string:tokens(RawLines, "\n"),
    FirstSetOfTiles = tile:parse(hd(SplitLines)),
    %FullSetOfTiles = tile:generate_rows(FirstSetOfTiles, 400000),
    %SafeTiles = lists:filter(fun(X) -> X == safe end, lists:flatten(FullSetOfTiles)),
    %?assertEqual(47, length(SafeTiles)).
    %SafeTileCount = lists:foldl(fun(X, Sum) -> length(lists:filter(fun(Y) -> Y == safe end, X)) + Sum end, 0, FullSetOfTiles),
    SafeTileCount = tile:generate_safe_tile_count_in_number_of_rows(FirstSetOfTiles, 400000),
    ?assertEqual(19993564, SafeTileCount).
