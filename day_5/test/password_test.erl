
-module(password_test).
-include_lib("eunit/include/eunit.hrl").

-import(password, [generate/1]).

should_generate_a_password_when_second_hash_is_correct_test() -> 
    RoomId = "abc",
    meck:new(md5),
    meck:sequence(md5, md5_hex, 1, ["000001", "000008", "00000f", "000004", "000007", "00000a", "000003", "000000" ]),
    ?assertEqual("18f47a30", generate(RoomId)),
    meck:unload(md5).
