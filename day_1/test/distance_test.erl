-module(distance_test).
-include_lib("eunit/include/eunit.hrl").

-import(distance, [distance/1]).

should_calculate_absolute_distance_from_origin_test() ->
    ?assertEqual(0, distance({0,0})),
    ?assertEqual(2, distance({-2,0})),
    ?assertEqual(2, distance({0,-2})),
    ?assertEqual(4, distance({-2,-2})),
    ?assertEqual(2, distance({2,0})),
    ?assertEqual(2, distance({0,2})),
    ?assertEqual(4, distance({2,2})).
