-module(password2).
-export([generate/1]).

hash(RoomId, Index) ->
    md5:md5_hex(RoomId ++ integer_to_list(Index)).

place_in_password(Index, _, Password) when Index > 7 -> Password;
place_in_password(Index, Key, Password) ->
    KeyInPassword = lists:nth(Index+1, Password),
    case KeyInPassword of
        $_ -> lists:sublist(Password,Index) ++ [Key] ++ lists:nthtail(Index+1,Password);
        _ -> Password
    end.

generate(RoomId) -> generate(RoomId, 0).

generate(RoomId, Index) -> generate(RoomId, Index, hash(RoomId, Index), [$_,$_,$_,$_,$_,$_,$_,$_]).

generate(_, _, _, [A,B,C,D,E,F,G,H|[]]=Password)
    when A =/= $_,
         B =/= $_,
         C =/= $_,
         D =/= $_,
         E =/= $_,
         F =/= $_,
         G =/= $_,
         H =/= $_ -> Password;
generate(RoomId, Index, [$0,$0,$0,$0,$0,PasswordIndex,Key|_],Password) -> 
    {PassIndex, _} = string:to_integer([PasswordIndex]),
    NewPassword = place_in_password(PassIndex, Key, Password),
    generate(RoomId, Index, Key, NewPassword);
generate(RoomId, Index, _, Password) -> 
    generate(RoomId, Index + 1, hash(RoomId, Index+1), Password).
