-module(room_checksum_test).
-include_lib("eunit/include/eunit.hrl").

-import(room_checksum, [generate/1]).

should_be_five_characters_long_test() -> 
    RoomName = "room-name",
    ?assertEqual(5, string:len(generate(RoomName))).

should_allow_dashes_in_room_name_test() ->
    ?assertEqual("abcde", generate("a-b-c-d-e")).

should_alphabetize_letters_of_same_count_test() ->
    ?assertEqual("abcde", generate("acebdf")).

should_order_by_frequency_of_occurence_test() ->
    ?assertEqual("eabcd", generate("abcdee")).
