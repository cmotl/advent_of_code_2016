-module(shift_cipher_test).
-include_lib("eunit/include/eunit.hrl").

-import(shift_cipher, [decrypt/2]).

should_retain_same_length_test() -> 
    RoomName = "room-name",
    ?assertEqual(9, string:len(decrypt(RoomName, 1))).

should_shift_dashes_to_spaces_test() -> 
    RoomName = "room-name-room",
    ?assertEqual("room name room", decrypt(RoomName, 0)).

should_shift_letters_without_wrap_around_test() -> 
    RoomName = "abcde",
    ?assertEqual("bcdef", decrypt(RoomName, 1)).

should_shift_letters_with_wrap_around_test() -> 
    RoomName = "abcde",
    ?assertEqual("abcde", decrypt(RoomName, 26)).

should_decrypt_real_phrase_test() -> 
    RoomName = "qzmt-zixmtkozy-ivhz",
    ?assertEqual("very encrypted name", decrypt(RoomName, 343)).
