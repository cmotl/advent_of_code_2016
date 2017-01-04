-module(decompressor2_test).
-include_lib("eunit/include/eunit.hrl").

-import(decompressor2, [decompress/1]).

should_decompress_string_with_no_markers_test() -> 
    ?assertEqual("ADVENT", decompress("ADVENT")).

should_decompress_string_with_one_marker_test() ->
    ?assertEqual("abbbbbc", decompress("a(1x5)bc")).

should_decompress_string_that_starts_with_one_marker_test() ->
    ?assertEqual("XYZXYZXYZ", decompress("(3x3)XYZ")).

should_decompress_string_with_two_markers_test() ->
    ?assertEqual("ABCBCDEFEFG", decompress("A(2x2)BCD(2x2)EFG")).
    
should_decompress_string_with_marker_inside_marker_2_test() ->
    ?assertEqual("XABCABCABCABCABCABCY", decompress("X(8x2)(3x3)ABCY")).
