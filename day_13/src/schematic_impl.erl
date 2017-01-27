-module(schematic_impl).
-export([binary_representation/1, number_of_one_bits/1, coordinate_precursor/2, schematic_element/3]).

binary_representation(Value) ->
    hd(io_lib:format("~.2B", [Value])).

map_bit($1) -> 1; 
map_bit($0) -> 0.

number_of_one_bits(Bits) ->
    lists:foldl(fun(Bit, Sum) -> Sum + map_bit(Bit) end, 0, Bits).

coordinate_precursor(X, Y) ->
    (X*X) + (3*X) + (2*X*Y) + (Y) + (Y*Y).

schematic_element(X,Y,FavoriteNumber) ->
    NumberOfBits = number_of_one_bits(binary_representation(coordinate_precursor(X,Y) + FavoriteNumber)),
    case NumberOfBits rem 2 of
        0 -> open_space;
        1 -> wall
    end.

