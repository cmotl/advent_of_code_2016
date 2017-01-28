-module(random_data).
-export([dragon_curve/1, dragon_curve/2, dragon_curve_checksum/1]).

invert(Segment) ->
    lists:map(fun(X) -> case X of $0 -> $1; $1 -> $0 end end, Segment).

dragon_curve(Input) ->
    lists:append([Input, "0", invert(lists:reverse(Input))]).

dragon_curve(Input, MaxSize) ->
    case length(Input) < MaxSize of
        true -> dragon_curve(dragon_curve(Input), MaxSize);
        false-> lists:sublist(Input, MaxSize)
    end.

dragon_curve_checksum([], Checksum) -> lists:reverse(Checksum);
dragon_curve_checksum([A,A|Rest], Checksum) -> 
    dragon_curve_checksum(Rest, [$1|Checksum]);
dragon_curve_checksum([_A,_B|Rest], Checksum) -> 
    dragon_curve_checksum(Rest, [$0|Checksum]).

dragon_curve_checksum(Input) ->
    Checksum = dragon_curve_checksum(Input, []),
    case length(Checksum) rem 2 of
        1 -> Checksum;
        0 -> dragon_curve_checksum(Checksum)
    end.
