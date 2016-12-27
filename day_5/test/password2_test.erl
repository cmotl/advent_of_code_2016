
-module(password2_test).
-include_lib("eunit/include/eunit.hrl").

-import(password2, [generate/1]).

should_generate_a_password_when_second_hash_is_correct_test() -> 
    RoomId = "abc",
    meck:new(md5),
    meck:sequence(md5, md5_hex, 1, ["0000005", "0000018", "000002f", "0000034", "0000047", "000005a", "0000063", "0000070" ]),
    ?assertEqual("58f47a30", generate(RoomId)),
    meck:unload(md5).
