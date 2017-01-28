-module(day_15_test).
-include_lib("eunit/include/eunit.hrl").

part1_integration_test() -> 
    Discs = [{7,0,1},{13,0,2},{3,2,3},{5,2,4},{17,0,5},{19,7,6}],
    TimeToDropThrough = kinetic_sculpture:time_to_release_capsule_for_fall_through(Discs),
    ?assertEqual(121834, TimeToDropThrough).

part2_integration_test() -> 
    Discs = [{7,0,1},{13,0,2},{3,2,3},{5,2,4},{17,0,5},{19,7,6},{11,0,7}],
    TimeToDropThrough = kinetic_sculpture:time_to_release_capsule_for_fall_through(Discs),
    ?assertEqual(3208099, TimeToDropThrough).
