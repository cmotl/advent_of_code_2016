-module(one_time_pad_test).
-include_lib("eunit/include/eunit.hrl").

-import(one_time_pad, [generate_hash/2, contains_triple/1, contains_quintuple/1, contains_exact_quintuple/2, is_key/2, quintuple_in_next_1000/3, generate_keys/3]).

should_generate_a_hash_test() ->
    Salt = "abc",
    Index = 0,
    ?assertEqual("577571be4de9dcce85a041ba0410f29f", generate_hash(Salt, Index)).

should_generate_a_hash_with_a_triple_test() ->
    Salt = "abc",
    Index = 18,
    ?assertEqual("0034e0923cc38887a57bd7b1d4f953df", generate_hash(Salt, Index)).

should_identify_a_triple_character_test() ->
    Hash = "0034e0923cc38887a57bd7b1d4f953df",
    ExpectedOutput = {true, $8},
    ?assertEqual(ExpectedOutput, contains_triple(Hash)).

should_identify_a_quintuple_character_test() ->
    Hash = "3aeeeee1367614f3061d165a5fe3cac3",
    ExpectedOutput = {true, $e},
    ?assertEqual(ExpectedOutput, contains_quintuple(Hash)).

should_identify_an_exact_quintuple_character_test() ->
    Hash = "3aeeeee1367614f3061d165a5fe3cac3",
    ExpectedOutput = true,
    ?assertEqual(ExpectedOutput, contains_exact_quintuple(Hash, $e)).


should_not_find_quintuple_in_next_1000_hashes_test() ->
    Hash = "0034e0923cc38887a57bd7b1d4f953df",
    Salt = "abc",
    Index = 18,
    Character = $8,
    ?assertEqual(false, quintuple_in_next_1000(Salt, Index, Character)).

should_find_quintuple_in_next_1000_hashes_test() ->
    Hash = "347dac6ee8eeea4652c7476d0f97bee5",
    Salt = "abc",
    Index = 39,
    Character = $e,
    ?assertEqual(true, quintuple_in_next_1000(Salt, Index, Character)).


should_identify_non_key_test() ->
    Salt = "abc",
    Index = 18,
    ?assertEqual(false, is_key(Salt, Index)).

should_identify_key_test() ->
    Salt = "abc",
    Index = 39,
    ?assertEqual(true, is_key(Salt, Index)).


should_find_desired_number_of_keys_test() ->
    Salt = "abc",
    Index = 0,
    NumberOfKeys = 2,
    Keys = generate_keys(Salt, Index, NumberOfKeys),
    ?assertEqual(92, hd(Keys)).
