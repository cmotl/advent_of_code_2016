-module(parse_directions_test).
-include_lib("eunit/include/eunit.hrl").

parse_direction_should_parse_left_turn_test() ->
    ?assertEqual({left,123}, parse_directions:parse_directions("L123")). 

parse_direction_should_parse_right_turn_test() ->
    ?assertEqual({right,2}, parse_directions:parse_directions("R2")). 
