-module(decompressor3_test).
-include_lib("eunit/include/eunit.hrl").

-import(decompressor3, [decompress/1]).

should_decompress_string_with_no_markers_test() -> 
    ?assertEqual(6, decompress("ADVENT")).

should_decompress_string_with_one_marker_test() ->
    ?assertEqual(7, decompress("a(1x5)bc")).

should_decompress_string_that_starts_with_one_marker_test() ->
    ?assertEqual(9, decompress("(3x3)XYZ")).

should_decompress_string_with_two_markers_test() ->
    ?assertEqual(11, decompress("A(2x2)BCD(2x2)EFG")).
    
should_decompress_string_with_marker_inside_marker_2_test() ->
    ?assertEqual(20, decompress("X(8x2)(3x3)ABCY")).

should_decompress_moderately_long_string_test() ->
    ?assertEqual(445, decompress("(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN")).

should_decompress_long_string_test() ->
    ?assertEqual(241920, decompress("(27x12)(20x12)(13x14)(7x10)(1x12)A")).
