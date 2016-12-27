-module(password).
-export([generate/1]).

hash(RoomId, Index) ->
    md5:md5_hex(RoomId ++ integer_to_list(Index)).

generate(RoomId) -> generate(RoomId, 0).

generate(RoomId, Index) -> generate(RoomId, Index, hash(RoomId, Index), []).

generate(_, _, _, [_,_,_,_,_,_,_,_|[]]=Password) -> Password;
generate(RoomId, Index, [$0,$0,$0,$0,$0,Key|_], Password) -> generate(RoomId, Index, [], Password ++ [Key]);
generate(RoomId, Index, _, Password) -> 
    generate(RoomId, Index + 1, hash(RoomId, Index+1), Password).
