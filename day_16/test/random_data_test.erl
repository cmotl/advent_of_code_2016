-module(random_data_test).
-include_lib("eunit/include/eunit.hrl").

-import(random_data, [dragon_curve/1, dragon_curve/2, dragon_curve_checksum/1]).

should_generate_a_dragon_curve_from_a_single_character_test() ->
    ?assertEqual("100", dragon_curve("1")),
    ?assertEqual("001", dragon_curve("0")).

should_generate_a_dragon_curve_from_multiple_characters_test() ->
    ?assertEqual("11111000000", dragon_curve("11111")),
    ?assertEqual("00000011111", dragon_curve("00000")).

should_generate_a_dragon_curve_from_multiple_different_characters_test() ->
    ?assertEqual("1111000010100101011110000", dragon_curve("111100001010")).

should_generate_a_dragon_curve_of_requested_size_test() ->
    ?assertEqual("1000110", dragon_curve("1", 7)).
    
should_generate_a_dragon_curve_up_to_requested_size_test() ->
    ?assertEqual("1000", dragon_curve("1", 4)).

should_generate_a_dragon_curve_checksum_for_two_identical_characters_test() ->
    ?assertEqual("1", dragon_curve_checksum("11")),
    ?assertEqual("1", dragon_curve_checksum("00")).

should_generate_a_dragon_curve_checksum_for_two_differing_characters_test() ->
    ?assertEqual("0", dragon_curve_checksum("10")),
    ?assertEqual("0", dragon_curve_checksum("01")).

should_generate_a_dragon_curve_checksum_that_is_odd_number_of_characters_test() ->
    ?assertEqual("0", dragon_curve_checksum("1110")).

should_generate_a_dragon_curve_checksum_from_multiple_reductions_test() ->
    ?assertEqual("100", dragon_curve_checksum("110010110100")).
