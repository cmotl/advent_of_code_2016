-module(keypad_test).
-include_lib("eunit/include/eunit.hrl").

-import(keypad,[map/2]).

should_map_the_1_key_and_directions_test() -> 
    ?assertEqual(1, map(1, up)),
    ?assertEqual(4, map(1, down)),
    ?assertEqual(1, map(1, left)),
    ?assertEqual(2, map(1, right)).

should_map_the_2_key_and_directions_test() -> 
    ?assertEqual(2, map(2, up)),
    ?assertEqual(5, map(2, down)),
    ?assertEqual(1, map(2, left)),
    ?assertEqual(3, map(2, right)).

should_map_the_3_key_and_directions_test() -> 
    ?assertEqual(3, map(3, up)),
    ?assertEqual(6, map(3, down)),
    ?assertEqual(2, map(3, left)),
    ?assertEqual(3, map(3, right)).

should_map_the_4_key_and_directions_test() -> 
    ?assertEqual(1, map(4, up)),
    ?assertEqual(7, map(4, down)),
    ?assertEqual(4, map(4, left)),
    ?assertEqual(5, map(4, right)).

should_map_the_5_key_and_directions_test() -> 
    ?assertEqual(2, map(5, up)),
    ?assertEqual(8, map(5, down)),
    ?assertEqual(4, map(5, left)),
    ?assertEqual(6, map(5, right)).

should_map_the_6_key_and_directions_test() -> 
    ?assertEqual(3, map(6, up)),
    ?assertEqual(9, map(6, down)),
    ?assertEqual(5, map(6, left)),
    ?assertEqual(6, map(6, right)).

should_map_the_7_key_and_directions_test() -> 
    ?assertEqual(4, map(7, up)),
    ?assertEqual(7, map(7, down)),
    ?assertEqual(7, map(7, left)),
    ?assertEqual(8, map(7, right)).

should_map_the_8_key_and_directions_test() -> 
    ?assertEqual(5, map(8, up)),
    ?assertEqual(8, map(8, down)),
    ?assertEqual(7, map(8, left)),
    ?assertEqual(9, map(8, right)).

should_map_the_9_key_and_directions_test() -> 
    ?assertEqual(6, map(9, up)),
    ?assertEqual(9, map(9, down)),
    ?assertEqual(8, map(9, left)),
    ?assertEqual(9, map(9, right)).
