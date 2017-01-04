-module(day_9_test).
-include_lib("eunit/include/eunit.hrl").

part1_integration_test() -> 
    {ok, Binary} = file:read_file("input.txt"),
    RawLines = binary:bin_to_list(Binary),
    SplitLines = string:tokens(RawLines, "\n"),
    DecompressedLines = lists:map(fun decompressor:decompress/1, SplitLines),
    ?assertEqual(150914, length(lists:flatten(DecompressedLines))).

part2_integration_test() -> 
    {ok, Binary} = file:read_file("input.txt"),
    RawLines = binary:bin_to_list(Binary),
    SplitLines = string:tokens(RawLines, "\n"),
    DecompressedLines = lists:map(fun decompressor3:decompress/1, SplitLines),
    ?assertEqual(11052855125, hd(lists:flatten(DecompressedLines))).

