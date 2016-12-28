-module(decoder_test).
-include_lib("eunit/include/eunit.hrl").

-import(decoder, [most_frequent_character/1, least_frequent_character/1, max_character/1, min_character/1]).

should_set_character_in_aggregate_when_seen_test() -> 
    ?assertEqual(nil, most_frequent_character([])).

should_find_single_character_as_most_frequent_test() -> 
    ?assertEqual($a, most_frequent_character("a")).

should_extract_nil_from_empty_dictionary_test() ->
    MaxCharacter = max_character(dict:new()),
    ?assertEqual(nil, MaxCharacter).

should_extract_single_character_from_dictionary_test() ->
    MaxCharacter = max_character(dict:from_list([{$a, 1}])),
    ?assertEqual($a, MaxCharacter).

should_extract_most_frequent_character_from_dictionary_test() ->
    MaxCharacter = max_character(dict:from_list([{$a, 1}, {$b, 2}])),
    ?assertEqual($b, MaxCharacter).

should_set_least_character_in_aggregate_when_seen_test() -> 
    ?assertEqual(nil, least_frequent_character([])).

should_find_least_single_character_as_most_frequent_test() -> 
    ?assertEqual($a, least_frequent_character("a")).

should_extract_min_nil_from_empty_dictionary_test() ->
    MinCharacter = min_character(dict:new()),
    ?assertEqual(nil, MinCharacter).

should_extract_min_single_character_from_dictionary_test() ->
    MinCharacter = min_character(dict:from_list([{$a, 1}])),
    ?assertEqual($a, MinCharacter).

should_extract_min_most_frequent_character_from_dictionary_test() ->
    MinCharacter = min_character(dict:from_list([{$a, 1}, {$b, 2}])),
    ?assertEqual($a, MinCharacter).

