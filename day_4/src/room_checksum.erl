-module(room_checksum).
-export([generate/1]).

s({K1,V1}, {K2,V2}) when V1 == V2 -> K1 < K2;
s({_,V1}, {_,V2}) -> V1 >= V2.

generate(RoomName) -> 
    TrimmedRoomName = re:replace(RoomName, "-", "", [global,{return, list}]),
    A = dict:to_list(lists:foldl(fun(K, D) -> dict:update(K, fun(V) -> V + 1 end, 1, D) end, dict:new(), TrimmedRoomName)),
    AlphaCount = lists:sort(fun s/2, A),
    Alpha = lists:map(fun({K,_}) -> K end, AlphaCount),
    string:substr(Alpha,1,5).
