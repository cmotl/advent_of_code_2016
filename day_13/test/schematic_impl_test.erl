-module(schematic_impl_test).
-include_lib("eunit/include/eunit.hrl").

-import(schematic_impl, [binary_representation/1, number_of_one_bits/1, coordinate_precursor/2, schematic_element/3]).

should_convert_decimal_integer_to_binary_representation_test() -> 
    ?assertEqual("10", binary_representation(2)).

should_count_number_of_1_bits_test() ->
    ?assertEqual(0, number_of_one_bits("0")),
    ?assertEqual(1, number_of_one_bits("1")),
    ?assertEqual(5, number_of_one_bits("11111")),
    ?assertEqual(3, number_of_one_bits("10101")).

should_calculate_coordinate_precursor_test() ->
    ?assertEqual(8, coordinate_precursor(1,1)),
    ?assertEqual(2, coordinate_precursor(0,1)), 
    ?assertEqual(4, coordinate_precursor(1,0)). 

should_map_coordinate_to_open_space_schematic_element_test() ->
    FavoriteNumber = 10, 
    X = 0, 
    Y = 0, 
    ?assertEqual(open_space, schematic_element(X,Y,FavoriteNumber)).

should_map_coordinate_to_wall_schematic_element_test() ->
    FavoriteNumber = 10, 
    X = 1, 
    Y = 0, 
    ?assertEqual(wall, schematic_element(X,Y,FavoriteNumber)).
