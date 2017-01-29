-module(room).
-export([surrounding_rooms/1, unlocked_surrounding_rooms/3, direction_to_path_code/1]).


surrounding_room_up({_X,1}) -> nil;
surrounding_room_up({X,Y}) -> {up, {X,Y-1}}.

surrounding_room_down({_X,4}) -> nil;
surrounding_room_down({X,Y}) -> {down, {X,Y+1}}.

surrounding_room_left({1,_Y}) -> nil;
surrounding_room_left({X,Y}) -> {left, {X-1,Y}}.

surrounding_room_right({4,_Y}) -> nil;
surrounding_room_right({X,Y}) -> {right, {X+1,Y}}.


surrounding_rooms(Room) ->
    SurroundingRooms = [surrounding_room_up(Room), surrounding_room_down(Room), surrounding_room_left(Room), surrounding_room_right(Room)],
    lists:filter(fun(X) -> X =/= nil end, SurroundingRooms).


locked_state(Door) when (Door >= $b) and (Door =< $f) -> unlocked;
locked_state(_) -> locked.

door_unlocked([unlocked,_,_,_], {up, _}) -> true;
door_unlocked([_,unlocked,_,_], {down, _}) -> true;
door_unlocked([_,_,unlocked,_], {left, _}) -> true;
door_unlocked([_,_,_,unlocked], {right, _}) -> true;
door_unlocked(_,_) -> false.

direction_to_path_code({up,_}) -> $U;
direction_to_path_code({down,_}) -> $D;
direction_to_path_code({left,_}) -> $L;
direction_to_path_code({right,_}) -> $R;
direction_to_path_code({start,_}) -> $S.

unlocked_surrounding_rooms(Passcode, History, Room) ->
    SurroundingRooms = surrounding_rooms(Room),
    PathCodes = lists:map( fun direction_to_path_code/1, History),
    FilteredPathCodes = lists:filter( fun(X) -> X =/= $S end, PathCodes),
    TotalPassCode = lists:append( [ Passcode, lists:reverse(FilteredPathCodes) ]),
    LockStates = lists:map(
        fun locked_state/1, 
        lists:sublist( md5:md5_hex(TotalPassCode), 4)
    ),
    lists:filter(fun(Door) -> door_unlocked(LockStates, Door) end, SurroundingRooms).

